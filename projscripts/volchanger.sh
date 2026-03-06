#!/usr/bin/env bash

devices=$(pw-dump | jq '.[] | select(.type == "PipeWire:Interface:Device") | {
    id: .id,
    name: .info.props["device.name"],
    description: .info.props["device.description"],
    ssid: .info.props["device.string"],
    active_profile: (.info.params.Profile // []) | first | {
        id: .index,
        name: .name,
        description: .description,
        available: .available
    },
    profiles: (
        (.info.params.EnumProfile // []) | map({
            id: .index,
            name: .name,
            description: .description,
            available: .available,
            # classes: .classes
        })
    )
}')

echo "$devices"
