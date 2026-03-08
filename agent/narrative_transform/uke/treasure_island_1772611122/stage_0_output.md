### PHASE A: EXTRACTION (What the text actually contains)

**A1. Anchor Inventory**

```
entities:
  - id: jim_hawkins
    name: Jim Hawkins
    type: person
    role: narrator, cabin-boy, innkeeper's son
  - id: mrs_hawkins
    name: Jim's Mother
    type: person
    role: innkeeper
  - id: billy_bones
    name: Billy Bones / "The Captain"
    type: person
    role: lodger, former pirate
  - id: dr_livesey
    name: Dr. Livesey
    type: person
    role: doctor, magistrate
  - id: squire_trelawney
    name: Squire Trelawney
    type: person
    role: landowner, financier of the expedition
  - id: ljs
    name: Long John Silver
    type: person
    role: ship's cook, mutineer leader
  - id: capt_smollett
    name: Captain Smollett
    type: person
    role: captain of the Hispaniola
  - id: pirates
    name: Flint's Crew / The Mutineers
    type: institution
    role: antagonists, seekers of treasure
  - id: gentry
    name: The Gentry Party
    type: institution
    role: protagonists, organizers of the expedition
  - id: ben_gunn
    name: Ben Gunn
    type: person
    role: marooned sailor

constraints:
  - id: c_venture
    description: A commercial venture requires owners to hire labor for a dangerous task where the rewards are not shared equally.
    agents: [squire_trelawney]
    targets: [pirates, jim_hawkins, capt_smollett]
  - id: c_pirate_code
    description: A group of sailors is bound by a past allegiance and a set of informal rules enforced by violence and superstition.
    agents: [ljs, pirates]
    targets: [pirates, billy_bones, gentry]
  - id: c_intimidation
    description: A man uses the threat of violence and his fearsome reputation to force a family to house and serve him without payment.
    agents: [billy_bones]
    targets: [jim_hawkins, mrs_hawkins]
  - id: c_honor
    description: An upper-class person must treat others as trustworthy if they behave according to certain social codes, regardless of their actual intent.
    agents: [gentry]
    targets: [gentry]
  - id: c_parole
    description: A person who has given their word is forbidden from escaping a dangerous situation, even when offered a chance.
    agents: [jim_hawkins]
    targets: [jim_hawkins]
  - id: c_cowardice
    description: A community of neighbors refuses to help a family in peril for fear of personal harm.
    agents: [The Hamlet's residents]
    targets: [jim_hawkins, mrs_hawkins]

tensions:
  - id: t_hierarchy
    poles: [capt_smollett, ljs]
    nature: The formal, hierarchical authority of a ship's captain is opposed by the charismatic, peer-based authority of a mutiny leader.
  - id: t_class
    poles: [squire_trelawney, pirates]
    nature: The legal right of an owner to the profits of a venture is opposed by the crew's belief that they have a right to the value their labor uncovers.
  - id: t_trust
    poles: [dr_livesey, ljs]
    nature: A system of social trust based on honor and appearances is exploited by a system of strategic deception.

absences:
  - id: a_state
    description: There is no effective state or legal authority capable of enforcing contracts or preventing mutiny on the high seas or the island.
  - id: a_alternative
    description: The narrative does not present a viable economic alternative for the sailors other than low-wage labor or high-risk piracy.
```

**A2. Source Domain**

```
primary_domain: governance
secondary_domains: [labor, economics, class, law]
disciplinary_lenses:
  - social contract theory (competing forms of social order)
  - game theory (strategic deception and cooperation between factions)
  - criminology (the structure and norms of a criminal subculture)
  - class analysis (conflict between capital owners and labor)
```

---

### PHASE B: DECOMPOSITION (Constraint properties)

**B1. Base Properties**

| id | Constraint Name | ε (est.) | Supp (est.) | Coord | Asymmetric |
| :--- | :--- | :--- | :--- | :--- | :--- |
| c_venture | Venture Asymmetry | 0.80 | 0.3 | true | true |
| c_pirate_code | Articles of Fortune | 0.85 | 0.9 | true | true |
| c_intimidation | Lodger's Terror | 0.90 | 0.8 | false | true |
| c_honor | Gentlemanly Trust | 0.10 | 0.2 | true | false |
| c_parole | Word as Bond | 0.05 | 0.1 | true | false |
| c_cowardice | Communal Fear | 0.70 | 0.1 | false | true |

**B2. Index Calculations**

**Constraint: c_venture (Venture Asymmetry)**, ε = 0.80
- `Squire Trelawney`: I=(institutional, biographical, arbitrage, regional). π=-0.2, σ=0.9.
  χ = 0.80 × -0.2 × 0.9 = -0.144. **Type: Rope**.
- `Captain Smollett`: I=(powerful, biographical, constrained, regional). π=0.6, σ=0.9.
  χ = 0.80 × 0.6 × 0.9 = 0.432. **Type: Rope** (He is a paid professional; the venture enables his work. χ is below the Tangled floor).
- `Jim Hawkins`: I=(powerless, biographical, trapped, regional). π=1.5, σ=0.9.
  χ = 0.80 × 1.5 × 0.9 = 1.08. **Type: Snare**.
- `A generic mutineer`: I=(moderate, biographical, constrained, regional). π=1.0, σ=0.9.
  χ = 0.80 × 1.0 × 0.9 = 0.72. **Type: Snare**.

**Constraint: c_honor (Gentlemanly Trust)**, ε = 0.10
- `Squire Trelawney`: I=(powerful, biographical, identity_locked, regional). π=0.6, σ=0.9.
  χ = 0.10 × 0.6 × 0.9 = 0.054. **Type: Rope**.
- `Dr. Livesey`: I=(analytical, biographical, mobile, regional). π=1.15, σ=0.9.
  χ = 0.10 × 1.15 × 0.9 = 0.1035. **Type: Rope**.
- `Long John Silver (exploiting it)`: I=(powerful, biographical, arbitrage, regional). π=0.6, σ=0.9.
  χ = 0.10 × 0.6 × 0.9 = 0.054. From his perspective, it's a tool, not a constraint he experiences. The low χ reflects its utility to him. **Type: Rope**.

**Constraint: c_pirate_code (Articles of Fortune)**, ε = 0.85
- `Long John Silver`: I=(powerful, biographical, arbitrage, regional). π=0.6, σ=0.9.
  χ = 0.85 × 0.6 × 0.9 = 0.459. **Type: Tangled Rope**.
- `George Merry (a mutineer)`: I=(moderate, biographical, identity_locked, regional). π=1.0, σ=0.9.
  χ = 0.85 × 1.0 × 0.9 = 0.765. **Type: Snare**.
- `Jim Hawkins (when captured)`: I=(powerless, biographical, trapped, regional). π=1.5, σ=0.9.
  χ = 0.85 × 1.5 × 0.9 = 1.1475. **Type: Snare**.

**B3. Constraint Graph**

- `c_venture`: `downstream_of`: none. `feeds_into`: `c_pirate_code` (as a reaction).
- `c_honor`: `downstream_of`: none (it's a pre-existing social code). `feeds_into`: `c_venture` (Trelawney's trust in Silver shapes the crew).
- `c_pirate_code`: `downstream_of`: `c_venture`. `feeds_into`: conflict with the venture's formal hierarchy.
- `c_intimidation`: `downstream_of`: `c_pirate_code` (Bones is hiding from it). `feeds_into`: the discovery of the map.
- `c_parole`: `downstream_of`: `c_honor`. `feeds_into`: Jim's capture.
- `c_cowardice`: `downstream_of`: `c_intimidation`. `feeds_into`: the Hawkinses' decision to act alone.

**B4. Dark Matter Probes**

- `Probe 1 — Operational Medium`: The concept of a "gentleman's word" or "honor" is treated as a natural law by the gentry, but is merely a tool or vulnerability to the pirates. This surfaces `c_honor`.
- `Probe 2 — Absence Inventory`: For the `c_pirate_code`, there is a conspicuous absence of any legitimate retirement plan or exit strategy. This ensures the code perpetuates itself through violence until its members are dead, making it a structural trap.
- `Probe 3 — Beneficiary Scan`: The framing of the conflict as "civilized men vs. savage pirates" benefits the gentry, as it morally legitimizes their claim to wealth acquired via a map stolen from a dead man. This framing obscures the underlying structural conflict of capital vs. labor, which is surfaced as `c_venture`.

**B5. False Mountain / Drift Check**

- `False Mountain`: The pirates' belief that their only option is to mutiny and that they are "gentlemen of fortune" by nature. This is presented as an immutable identity, but it's a direct result of the extractive structure of the venture (`c_venture`), which offers them no legitimate path to significant wealth.

---

### PHASE C: SELECTION (Three-axis budget)

**C1. Centrality Scoring**

| id | Constraint Name | Inbound | Outbound | Type Weight (Avg) | Centrality |
| :--- | :--- | :--- | :--- | :--- | :--- |
| c_pirate_code | Articles of Fortune | 1 | 1 | 2.5 (Snare/Tangled) | 4.5 |
| c_venture | Venture Asymmetry | 0 | 1 | 2 (Snare/Rope) | 3.0 |
| c_honor | Gentlemanly Trust | 0 | 1 | 1 (Rope) | 2.0 |
| c_intimidation | Lodger's Terror | 1 | 1 | 2 (Snare) | 4.0 |

*Note: `c_pirate_code` is the most central downstream effect, a tangled rope/snare that drives the main conflict.*

**C2. Selection Algorithm**

1.  **Select highest-centrality:** `c_pirate_code` (Articles of Fortune). It is a Tangled Rope for its leader and a Snare for its members, making it the core of the conflict.
2.  **Select most distinct upstream dependency:** `c_venture` (Venture Asymmetry) is the direct upstream cause. It operates on the level of economic structure, while `c_pirate_code` operates on the level of subcultural governance. Their ε values are similar, but their observables are very different (hiring practices vs. secret oaths).
3.  **Select next highest-centrality with different observable:** `c_intimidation` is high but is an early plot driver. `c_honor` (Gentlemanly Trust) is a better choice. It operates on a completely different axis of social norms and trust, not economic or coercive power. Its beneficiaries (gentry, amongst themselves) and victims (gentry, when facing pirates) are distinct. It explains *how* the pirates were able to infiltrate the venture.

**C3. Generation Order**

1.  `c_venture` (Venture Asymmetry) is the foundational economic structure. (Order 1)
2.  `c_honor` (Gentlemanly Trust) is the social lubricant/vulnerability that allows the conflict to be set up. It is independent of the venture in origin but interacts with it. (Order 2)
3.  `c_pirate_code` (Articles of Fortune) is the direct antagonistic response to the venture's structure, enabled by the vulnerability of the honor code. It is downstream of both. (Order 3)

**C4. Deferred Constraints**

- `c_intimidation`: Billy Bones's terror at the inn functions as the inciting incident, introducing the violence and history that underpins the treasure hunt.
- `c_parole`: Jim's self-imposed restriction functions as a plot device to place him in the pirates' camp, demonstrating his maturation into the gentry's honor system.
- `c_cowardice`: The villagers' refusal to help isolates the protagonists, forcing them into self-reliance and establishing the story's theme of small, competent groups against larger, disorganized ones.

---

### PHASE D: MANIFEST OUTPUT

```xml
<constraint_manifest>
  <selected>
    <constraint id="C1" name="VentureAsymmetry" generation_order="1">
      <base_properties>
        <epsilon>0.80</epsilon>
        <suppression>0.30</suppression>
        <coordination>true</coordination>
        <asymmetric>true</asymmetric>
      </base_properties>
      <graph>
        <downstream_of>none</downstream_of>
        <feeds_into>C3</feeds_into>
      </graph>
      <character_classifications>
        <character name="Squire Trelawney">
          <index>
            <power>institutional</power>
            <time>biographical</time>
            <exit>arbitrage</exit>
            <scope>regional</scope>
          </index>
          <chi>-0.144</chi>
          <type>Rope</type>
        </character>
        <character name="A generic mutineer">
          <index>
            <power>moderate</power>
            <time>biographical</time>
            <exit>constrained</exit>
            <scope>regional</scope>
          </index>
          <chi>0.72</chi>
          <type>Snare</type>
        </character>
        <character name="Jim Hawkins">
          <index>
            <power>powerless</power>
            <time>biographical</time>
            <exit>trapped</exit>
            <scope>regional</scope>
          </index>
          <chi>1.08</chi>
          <type>Snare</type>
        </character>
      </character_classifications>
      <indexical_variance>The same venture is a Rope for its owner (enabling wealth creation) but a Snare for the hired labor (a high-risk trap with unequal rewards).</indexical_variance>
      <selection_reason>This is the foundational economic constraint that creates the conditions for mutiny by establishing a high-stakes goal with an inherently unstable class structure.</selection_reason>
    </constraint>
    <constraint id="C2" name="GentlemanlyTrust" generation_order="2">
      <base_properties>
        <epsilon>0.10</epsilon>
        <suppression>0.20</suppression>
        <coordination>true</coordination>
        <asymmetric>false</asymmetric>
      </base_properties>
      <graph>
        <downstream_of>none</downstream_of>
        <feeds_into>C1</feeds_into>
      </graph>
      <character_classifications>
        <character name="Squire Trelawney">
          <index>
            <power>powerful</power>
            <time>biographical</time>
            <exit>identity_locked</exit>
            <scope>regional</scope>
          </index>
          <chi>0.054</chi>
          <type>Rope</type>
        </character>
        <character name="Dr. Livesey">
          <index>
            <power>analytical</power>
            <time>biographical</time>
            <exit>mobile</exit>
            <scope>regional</scope>
          </index>
          <chi>0.1035</chi>
          <type>Rope</type>
        </character>
      </character_classifications>
      <indexical_variance>None among its adherents. The constraint is a functional Rope for those within the gentry class, but it functions as a systemic vulnerability when interfacing with those who do not share its values (the pirates).</indexical_variance>
      <selection_reason>This constraint explains the mechanism of infiltration. It operates on the axis of social norms, contrasting with the economic and coercive logic of the other two constraints, and is the critical vulnerability that allows the conflict to begin.</selection_reason>
    </constraint>
    <constraint id="C3" name="ArticlesOfFortune" generation_order="3">
      <base_properties>
        <epsilon>0.85</epsilon>
        <suppression>0.90</suppression>
        <coordination>true</coordination>
        <asymmetric>true</asymmetric>
      </base_properties>
      <graph>
        <downstream_of>C1</downstream_of>
        <feeds_into>none</feeds_into>
      </graph>
      <character_classifications>
        <character name="Long John Silver">
          <index>
            <power>powerful</power>
            <time>biographical</time>
            <exit>arbitrage</exit>
            <scope>regional</scope>
          </index>
          <chi>0.459</chi>
          <type>Tangled Rope</type>
        </character>
        <character name="George Merry">
          <index>
            <power>moderate</power>
            <time>biographical</time>
            <exit>identity_locked</exit>
            <scope>regional</scope>
          </index>
          <chi>0.765</chi>
          <type>Snare</type>
        </character>
        <character name="Jim Hawkins">
          <index>
            <power>powerless</power>
            <time>biographical</time>
            <exit>trapped</exit>
            <scope>regional</