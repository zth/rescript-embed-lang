var path = require("path");
var cp = require("child_process");
var fs = require("fs");
var platform = process.platform;

/**
 * Since os.arch returns node binary's target arch, not
 * the system arch.
 * Credits: https://github.com/feross/arch/blob/af080ff61346315559451715c5393d8e86a6d33c/index.js#L10-L58
 */

function ppxArch(targetPlatform = platform, targetArch = process.arch) {
  if (targetArch === "arm64") {
    return "arm64";
  }

  /**
   * The running binary is 64-bit, so the OS is clearly 64-bit.
   */
  if (targetArch === "x64") {
    return "x64";
  }

  /**
   * All recent versions of Mac OS are 64-bit.
   */
  if (targetPlatform === "darwin") {
    return "x64";
  }

  /**
   * On Windows, the most reliable way to detect a 64-bit OS from within a 32-bit
   * app is based on the presence of a WOW64 file: %SystemRoot%\SysNative.
   * See: https://twitter.com/feross/status/776949077208510464
   */
  if (targetPlatform === "win32") {
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
  if (targetPlatform === "linux") {
    var output = cp.execSync("getconf LONG_BIT", { encoding: "utf8" });
    return output === "64\n" ? "x64" : "x86";
  }

  /**
   * If none of the above, assume the architecture is 32-bit.
   */
  return "x86";
}

function getBinaryPlatform(targetPlatform, architecture) {
  switch (targetPlatform) {
    case "win32":
      if (architecture !== "x64") {
        throw new Error("x86 is currently not supported on Windows");
      }
      return "windows-latest";
    case "linux":
      if (architecture === "arm64") {
        return "linux-arm64";
      }
      if (architecture === "x64") {
        return "linux";
      }
      throw new Error(architecture + " is currently not supported on Linux");
    case "darwin":
      return architecture === "arm64" ? "macos-arm64" : "macos-latest";
    default:
      throw new Error("no release built for the " + targetPlatform + " platform");
  }
}

function copyPlatformBinaries(platform, baseDirectory = __dirname) {
  /**
   * Copy the PPX
   */
  const ppxFinalFilename = platform === "windows-latest" ? "ppx.exe" : "ppx";
  const ppxFinalPath = path.join(baseDirectory, ppxFinalFilename);

  if (!fs.existsSync(ppxFinalPath)) {
    fs.copyFileSync(path.join(baseDirectory, "ppx-" + platform), ppxFinalPath);
  }
  fs.chmodSync(ppxFinalPath, 0o777);

  if (platform === "windows-latest") {
    const extensionlessPpxPath = path.join(baseDirectory, "ppx");

    if (!fs.existsSync(extensionlessPpxPath)) {
      fs.copyFileSync(ppxFinalPath, extensionlessPpxPath);
    }
    fs.chmodSync(extensionlessPpxPath, 0o777);
  }
}

function unlinkIfExistsSync(path) {
  if (fs.existsSync(path)) {
    fs.unlinkSync(path);
  }
}

function removeInitialBinaries(baseDirectory = __dirname) {
  unlinkIfExistsSync(path.join(baseDirectory, "ppx-macos-arm64"));
  unlinkIfExistsSync(path.join(baseDirectory, "ppx-macos-latest"));
  unlinkIfExistsSync(path.join(baseDirectory, "ppx-windows-latest"));
  unlinkIfExistsSync(path.join(baseDirectory, "ppx-linux"));
  unlinkIfExistsSync(path.join(baseDirectory, "ppx-linux-arm64"));
}

function main() {
  try {
    copyPlatformBinaries(getBinaryPlatform(platform, ppxArch()));
    removeInitialBinaries();
  } catch (error) {
    console.warn("error: " + error.message);
    process.exitCode = 1;
  }
}

if (require.main === module) {
  main();
}

module.exports = {
  copyPlatformBinaries,
  getBinaryPlatform,
  ppxArch,
  removeInitialBinaries,
};
