var path = require("path");
var cp = require("child_process");
var fs = require("fs");
var platform = process.platform;

/**
 * Since os.arch returns node binary's target arch, not
 * the system arch.
 * Credits: https://github.com/feross/arch/blob/af080ff61346315559451715c5393d8e86a6d33c/index.js#L10-L58
 */

function ppxArch() {
  if (platform === "darwin" && process.arch === "arm64") {
    return "arm64";
  }

  /**
   * The running binary is 64-bit, so the OS is clearly 64-bit.
   */
  if (process.arch === "x64") {
    return "x64";
  }

  /**
   * All recent versions of Mac OS are 64-bit.
   */
  if (process.platform === "darwin") {
    return "x64";
  }

  /**
   * On Windows, the most reliable way to detect a 64-bit OS from within a 32-bit
   * app is based on the presence of a WOW64 file: %SystemRoot%\SysNative.
   * See: https://twitter.com/feross/status/776949077208510464
   */
  if (process.platform === "win32") {
    var useEnv = false;
    try {
      useEnv = !!(
        process.env.SYSTEMROOT && fs.statSync(process.env.SYSTEMROOT)
      );
    } catch (err) {}

    var sysRoot = useEnv ? process.env.SYSTEMROOT : "C:\\Windows";

    // If %SystemRoot%\SysNative exists, we are in a WOW64 FS Redirected application.
    var isWOW64 = false;
    try {
      isWOW64 = !!fs.statSync(path.join(sysRoot, "sysnative"));
    } catch (err) {}

    return isWOW64 ? "x64" : "x86";
  }

  /**
   * On Linux, use the `getconf` command to get the architecture.
   */
  if (process.platform === "linux") {
    var output = cp.execSync("getconf LONG_BIT", { encoding: "utf8" });
    return output === "64\n" ? "x64" : "x86";
  }

  /**
   * If none of the above, assume the architecture is 32-bit.
   */
  return "x86";
}

function copyPlatformBinaries(platform) {
  /**
   * Copy the PPX
   */
  const ppxFinalFilename = platform === "windows-latest" ? "ppx.exe" : "ppx";
  const ppxFinalPath = path.join(__dirname, ppxFinalFilename);

  if (!fs.existsSync(ppxFinalPath)) {
    fs.copyFileSync(path.join(__dirname, "ppx-" + platform), ppxFinalPath);
  }
  fs.chmodSync(ppxFinalPath, 0o777);
}

function unlinkIfNotExistsSync(path) {
  if (fs.existsSync(path)) {
    fs.unlinkSync(path);
  }
}

function removeInitialBinaries() {
  unlinkIfNotExistsSync(path.join(__dirname, "ppx-macos-arm64"));
  unlinkIfNotExistsSync(path.join(__dirname, "ppx-macos-latest"));
  unlinkIfNotExistsSync(path.join(__dirname, "ppx-windows-latest"));
  unlinkIfNotExistsSync(path.join(__dirname, "ppx-linux"));
}

switch (platform) {
  case "win32": {
    if (ppxArch() !== "x64") {
      console.warn("error: x86 is currently not supported on Windows");
      process.exit(1);
    }
    copyPlatformBinaries("windows-latest");
    break;
  }
  case "linux":
    copyPlatformBinaries(platform);
    break;
  case "darwin": {
    if (ppxArch() === "arm64") {
      copyPlatformBinaries("macos-arm64");
    } else {
      copyPlatformBinaries("macos-latest");
    }
    break;
  }
  default:
    console.warn("error: no release built for the " + platform + " platform");
    process.exit(1);
}

removeInitialBinaries();
