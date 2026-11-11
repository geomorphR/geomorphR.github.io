param(
    [Parameter(Mandatory = $true)]
    [ValidateSet('Advertisement', 'Materials')]
    [string]$Mode
)
$ErrorActionPreference = 'Stop'
$sourceName = if ($Mode -eq 'Advertisement') { 'Barcelona.html' } else { 'materials.html' }
$sourcePath = Join-Path $PSScriptRoot $sourceName
$destinationPath = Join-Path $PSScriptRoot 'index.html'
if (-not (Test-Path -LiteralPath $sourcePath -PathType Leaf)) {
    throw "Cannot switch homepage: $sourceName is missing."
}
Copy-Item -LiteralPath $sourcePath -Destination $destinationPath -Force
Write-Output "Workshop homepage now shows: $Mode"
