% ============================================================================
% CONSTRAINT STORY: paris_article_4_ndc__supranational_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_paris_article_4_ndc__supranational_reading, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Constraint Identity Rule (DP-001: ε-Invariance) ---
% Each constraint story must have a single, stable base extractiveness (ε).
% If changing the observable used to evaluate this constraint would change ε,
% you are looking at two distinct constraints. Write separate .pl files for
% each, link them with affects_constraint/2, and document the relationship
% in both files' narrative context sections.
%
% The context tuple is CLOSED at arity 4: (P, T, E, S).
% Do not add measurement_basis, beneficiary/victim, or any other arguments.
% Linter Rule 23 enforces context/4.
%
% See: epsilon_invariance_principle.md

% --- Namespace Hooks (Required for loading) ---
:- multifile
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:has_sunset_clause/1,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: paris_article_4_ndc__supranational_reading
 *   human_readable: Paris Article 4 NDC Binding Ratchet (Supranational Reading)
 *   domain: international/environmental/political-economy
 *
 * SUMMARY:
 *   This constraint instantiates the supranational reading of Article 4 of
 *   the Paris Agreement: NDCs (Nationally Determined Contributions) are
 *   binding international commitments on a ratcheting trajectory toward
 *   net-zero emissions with supranational accountability enforced via IPCC
 *   climate science benchmarks, UNFCCC transparency rules, and climate
 *   litigation. Under this reading, states cannot opt for sovereignty buffers
 *   or differentiated self-pacing—the 1.5°C target and measurable NDC
 *   adequacy are empirically fixed by climate physics, not negotiable. The
 *   constraint extracts heavily from carbon-intensive nations and fossil-fuel
 *   industries (whose regulatory landscape is deterministically ordered
 *   toward obsolescence) and distributes gains to clean-energy industries,
 *   climate-vulnerable populations (notionally), and institutional actors
 *   that operationalize the supranational framework (IPCC, courts,
 *   multilateral banks). Energy-poor developing states occupy an ambiguous
 *   position: nominally beneficiaries of climate finance and adaptation
 *   funds, but actually structurally locked into ratcheting reduction targets
 *   that arrive before capital does—making them simultaneously payers and
 *   excluded beneficiaries.
 *
 * KEY AGENTS:
 *   - ipcc_unfccc_authority: sets the empirical baseline (1.5°C science) and operationalizes binding interpretation via transparency frameworks
 *   - carbon_intensive_nations: face ratcheting regulatory burden, exit foreclosed by supranational science consensus
 *   - fossil_fuel_industries: face regulatory extinction, trapped by identity (carbon-extraction business model incompatible with ratcheting)
 *   - developing_states_with_energy_poverty: structurally ambiguous—beneficiaries in theory (climate finance), payers in practice (ratcheting targets precede capital)
 *   - clean_energy_industries: structured into profitability by the constraint's binding ratchet
 *   - climate_litigation_coalition: benefits from supranational reading's justiciability, weaponizes it into national enforcement
 *   - equity_advocates: excluded from blocking, object to uniform ratcheting without differentiated capacity
 *   - sovereigntist_governments: excluded by supranational reading's empirical anchoring, cannot credibly claim voluntariness
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(paris_article_4_ndc__supranational_reading, 0.82).
domain_priors:suppression_score(paris_article_4_ndc__supranational_reading, 0.71).
domain_priors:theater_ratio(paris_article_4_ndc__supranational_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(paris_article_4_ndc__supranational_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(paris_article_4_ndc__supranational_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(paris_article_4_ndc__supranational_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(paris_article_4_ndc__supranational_reading, accessibility_collapse, 0.79).
narrative_ontology:constraint_metric(paris_article_4_ndc__supranational_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(paris_article_4_ndc__supranational_reading, tangled_rope).
narrative_ontology:human_readable(paris_article_4_ndc__supranational_reading, "Paris Article 4 NDC Binding Ratchet (Supranational Reading)").
narrative_ontology:topic_domain(paris_article_4_ndc__supranational_reading, "international/environmental/political-economy").

domain_priors:requires_active_enforcement(paris_article_4_ndc__supranational_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(paris_article_4_ndc__supranational_reading, '444946ba-c626-4844-a261-dcecdaccbce2').
narrative_ontology:cs_kernel_codification('444946ba-c626-4844-a261-dcecdaccbce2', fixed_text).
narrative_ontology:cs_authority_grounding('444946ba-c626-4844-a261-dcecdaccbce2', extraction).
narrative_ontology:cs_interpretation_layer_present('444946ba-c626-4844-a261-dcecdaccbce2').
narrative_ontology:cs_reading_relation('444946ba-c626-4844-a261-dcecdaccbce2', paris_article_4_ndc__sovereigntist_reading, forecloses).
narrative_ontology:cs_reading_relation('444946ba-c626-4844-a261-dcecdaccbce2', paris_article_4_ndc__equity_reading, coexists_with).
narrative_ontology:cs_axiom('444946ba-c626-4844-a261-dcecdaccbce2', foundational, binding_empirical_ndc_ratchet).
narrative_ontology:cs_axiom_status(binding_empirical_ndc_ratchet, holdable).
narrative_ontology:cs_axiom_grounding('444946ba-c626-4844-a261-dcecdaccbce2', binding_empirical_ndc_ratchet, empirically_contingent).
narrative_ontology:cs_axiom('444946ba-c626-4844-a261-dcecdaccbce2', foundational, supranational_transparency_accountability).
narrative_ontology:cs_axiom_status(supranational_transparency_accountability, holdable).
narrative_ontology:cs_axiom_grounding('444946ba-c626-4844-a261-dcecdaccbce2', supranational_transparency_accountability, conventional).
narrative_ontology:cs_reference_frame('444946ba-c626-4844-a261-dcecdaccbce2', paris_agreement_binding_ratchet_2015).
narrative_ontology:cs_drift_state('444946ba-c626-4844-a261-dcecdaccbce2', contemporary_2025_ndc_cycle, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('444946ba-c626-4844-a261-dcecdaccbce2', '').
narrative_ontology:cs_kernel_id(paris_article_4_ndc__supranational_reading, paris_article_4_ndc).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(paris_article_4_ndc__supranational_reading, global_climate_vulnerable_states).
narrative_ontology:constraint_beneficiary(paris_article_4_ndc__supranational_reading, clean_energy_industries).
narrative_ontology:constraint_beneficiary(paris_article_4_ndc__supranational_reading, climate_litigation_coalition).
narrative_ontology:constraint_victim(paris_article_4_ndc__supranational_reading, carbon_intensive_nations).
narrative_ontology:constraint_victim(paris_article_4_ndc__supranational_reading, fossil_fuel_industries).
narrative_ontology:constraint_victim(paris_article_4_ndc__supranational_reading, developing_states_with_energy_poverty).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(paris_article_4_ndc__supranational_reading, developing_states_with_energy_poverty).
narrative_ontology:constraint_vindicates(paris_article_4_ndc__supranational_reading, supranational_climate_authority_doctrine).
narrative_ontology:constraint_vindicates(paris_article_4_ndc__supranational_reading, international_emissions_accountability).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the baseline climate science (1.5–2.0°C targets), interprets NDC adequacy, and administers the transparency framework that measures compliance. Controls the narrative of what counts as binding, sufficient, and ratcheting. Issues guidance that operationalizes the supranational reading through IPCC assessment cycles and UNFCCC rulebooks.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__supranational_reading, ipcc_unfccc_authority, agenda_setter,
    institutional, generational, analytical, universal).

% Face ratcheting emissions reduction targets anchored to 1.5°C science. Domestic energy systems engineered for carbon infrastructure; exit from coal, oil, gas requires capital destruction and workforce dislocation. International pressure (Paris Agreement text, IPCC guidance, litigation, sanctions) forecloses the sovereigntist reading—they cannot credibly claim NDCs are voluntary when every climate forum calls them binding and legally reviewable.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__supranational_reading, carbon_intensive_nations, payer,
    powerful, generational, constrained, national).

% Face regulatory extinction as NDCs operationalize into national carbon pricing, phase-out timelines, and stranded-asset provisions. Their entire operational logic—profitability contingent on carbon-intensive extraction—is incompatible with binding ratcheting. Exit would require abandoning corporate identity and shareholder model; they remain structurally locked in, opposing the constraint even as it tightens.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__supranational_reading, fossil_fuel_industries, payer,
    powerful, biographical, identity_locked, global).

% Face ratcheting reduction targets that conflict with energy access mandates (SDG 7). Their energy poverty (400M+ without electricity) cannot be solved through renewables alone without massive capital—yet the supranational reading defines development energy as a loss against the global carbon budget. Nominally beneficiaries of climate finance, but binding targets arrive before finance materializes; their only exit is to be reclassified as developed (identity_locked), which triggers harsher targets.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__supranational_reading, developing_states_with_energy_poverty, payer,
    moderate, biographical, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(paris_article_4_ndc__supranational_reading, developing_states_with_energy_poverty, beneficiary).

% Island nations, least-developed countries bearing existential climate risk despite near-zero historical emissions. Trapped in the supranational system as beneficiaries in theory (adaptation finance, loss-and-damage funds); in practice they receive 5–15% of promised climate finance and vote with no blocking power in UNFCCC. Their exit is literal disappearance (island submerged); they remain in the constraint by physical necessity, not choice.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__supranational_reading, global_climate_vulnerable_states, beneficiary,
    powerless, immediate, trapped, global).

% Structured into profitability by the constraint's binding ratchet: every NDC that commits to emissions reduction creates demand for solar, wind, batteries, grid infrastructure. The supranational reading guarantees these markets grow globally and asymmetrically in developed nations (capital availability, grid capacity). They benefit from the constraint's enforcement without bearing its costs; exit means reverting to pre-constraint flat markets.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__supranational_reading, clean_energy_industries, beneficiary,
    powerful, generational, arbitrage, global).

% NGOs, plaintiffs, lawyers, and courts weaponizing the supranational reading through litigation (Netherlands v. State, Neubauer v. Germany, Juliana v. USA). The more binding the NDC reading, the more justiciable it becomes, and the more litigation revenue and institutional power flow to the coalition. They benefit from ratcheting supranational authority by converting it into enforceable legal duties at national scope.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__supranational_reading, climate_litigation_coalition, beneficiary,
    organized, generational, arbitrage, national).

% Argue that the supranational reading erases differentiated responsibilities—that applying uniform ratcheting to developed and developing states reproduces colonialism by forcing energy-poor nations to sacrifice development on the altar of global carbon budgets set by high-emitters historically. They are structurally excluded from UNFCCC decision-making by consensus rules weighted toward incumbent power; their objection is formally heard but never binding.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__supranational_reading, equity_advocates, excluded,
    organized, generational, constrained, global).

% Maintain that NDCs are voluntary self-determined pledges; the supranational reading treats them as binding international law. They argue for CBDR (Common But Differentiated Responsibilities) as a buffer—developed nations bear the ratchet, developing nations self-pace. The supranational reading forecloses this via IPCC science and UNFCCC transparency rules that make NDC adequacy objectively measurable against 1.5°C, removing the sovereignty buffer.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__supranational_reading, sovereigntist_governments, excluded,
    powerful, biographical, constrained, national).

% Central banks, multilaterals, and rating agencies use the supranational reading to price climate risk into sovereign and corporate debt. The more binding the NDC reading, the more asset stranding and refinancing pressure they can justify, which enables divestment from carbon assets and reallocation to green bonds. They occupy the observational seat—neither paying nor benefiting directly—but their pricing rules operationalize the constraint's extraction mechanism into capital markets.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__supranational_reading, financial_institutions, observer,
    institutional, biographical, arbitrage, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(paris_article_4_ndc__supranational_reading, clean_energy_industries).
narrative_ontology:fixing_cost_class(paris_article_4_ndc__supranational_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates global emissions reduction toward a stabilized climate state (1.5–2.0°C) by binding all parties to transparent, ratcheting reduction pathways. Solves the tragedy-of-the-commons problem of carbon atmosphere: without binding coordination, each state free-rides on others' reductions.
% TRANSFER_FUNCTION: Transfers regulatory burden (carbon reduction requirements, stranded asset losses, workforce transition costs) from historically high-emitting developed nations to current emitters (carbon-intensive nations regardless of development status); transfers capital from incumbent fossil-fuel infrastructure to clean-energy industries and climate-vulnerable adaptation. Institutionalizes wealth transfer from North-to-South via climate finance and loss-and-damage funds, though actual capital flows lag formal commitments by 4–10× ratios.
% ABSENT_VOICES: Equity advocates and affected developing populations objecting to uniform ratcheting without differential capacity; fossil-fuel workers and communities facing dislocation; future generations locked into carbon budgets authored by current negotiators. Sovereigntist governments formally present but structurally excluded from blocking via supranational reading's empirical accountability (1.5°C science cannot be negotiated away).
% DISAPPEARANCE_RATIONALE: If binding NDCs and supranational accountability vanished, energy markets would revert to fossil-fuel optimization (stable relative prices, no stranded assets), carbon-intensive nations would face no ratcheting pressure, climate litigation would lose its justiciability anchor, clean-energy industries would lose their regulatory growth driver, and global emissions trajectories would likely stabilize 2–3°C warmer than the constraint produces. The atmospheric carbon budget becomes a commons again; state energy sovereignty becomes unconstrained.
% FOUNDING_PROBLEM: The Paris Agreement (2015) faced the foundational design choice: are NDCs binding international commitments with enforcement mechanisms, or voluntary self-determined pledges preserved in sovereignty? The supranational reading resolves this via operationalization: IPCC science anchors NDC adequacy to 1.5°C (making the target empirically non-negotiable), UNFCCC transparency rules make NDC implementation measurable and reviewable, and climate litigation converts these into judicially enforceable standards—transforming NDCs from pledges into de facto binding international law through institutional layering rather than formal amendment.
% FOUNDING_PROBLEM_CORROBORATION: IPCC Assessment Reports (AR6, 2021) and UNFCCC Technical Dialogues (2023–2025) authored by independent climate scientists corroborate that 1.5°C feasibility requires immediate binding action and ratcheting. Conversely, major carbon-intensive states and fossil-fuel-aligned analysts contest whether NDCs are binding or remain pledges; they argue the 1.5°C target is political, not scientific necessity. Equity advocates and development economists (UN UNCTAD, African Union) corroborate that supranational ratcheting excludes differentiation—the founding problem is real but the supranational solution is contested as unjust.
narrative_ontology:disappearance_verdict(paris_article_4_ndc__supranational_reading, world_rearranges).
narrative_ontology:founding_problem_status(paris_article_4_ndc__supranational_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(paris_article_4_ndc__supranational_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(paris_article_4_ndc__supranational_reading, 'none', 1).
narrative_ontology:epsilon_provenance(paris_article_4_ndc__supranational_reading, 0.82, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(paris_article_4_ndc__supranational_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(paris_article_4_ndc__supranational_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(paris_article_4_ndc__supranational_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.82) because the constraint's operation systematically transfers regulatory burden from historically high-emitters to current emitters, capital from fossil infrastructure to clean energy, and enforces this transfer via institutional layering (IPCC science → UNFCCC rules → national carbon pricing → litigation). The extraction is not negotiable because it is anchored to empirical climate physics—1.5°C feasibility is not a preference but a physical constraint on the carbon budget. Suppression is substantial (0.71) because the constraint's persistence depends on actively defending its empirical anchoring and excluding sovereigntist alternatives; fossil-fuel states and industries mount continuous resistance to the 'binding' interpretation, yet the supranational reading's institutional operationalization (transparency rules, litigation, capital markets pricing) suppresses that resistance by making exit paths structurally unavailable. Theater ratio is moderate (0.28): genuine climate coordination is happening (emissions are being reduced in response to NDC commitment), but a growing share of institutional activity is performative (countries submitting inadequate NDCs they know won't meet 1.5°C, governments announcing net-zero 2050 targets while expanding fossil extraction, climate finance pledges that never materialize). The measurement series shows extractiveness and suppression rising sharply 0–15, then plateauing 15–30 as the institutional framework stabilizes and major non-compliance is visible.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and beneficiary/agenda-setter seats compute radically different types. From the IPCC/UNFCCC seat, this constraint is genuine coordination: it solves the tragedy-of-the-commons carbon problem by operationalizing binding, transparent, ratcheting reduction pathways. From the carbon-intensive nation and fossil-fuel seats, this constraint is extraction: they are forced into regulatory transition, capital destruction, and workforce dislocation by an authority structure (IPCC science + UNFCCC rules + litigation) they cannot renegotiate or escape. The supranational reading instantiates the beneficiary/agenda-setter framing and empirically forecloses the sovereigntist alternative by making 1.5°C non-negotiable. Developing states see a third perspective: they are told they are beneficiaries (entitled to climate finance and adaptation), but structurally they are payers (ratcheting targets that precede capital) with identity-locked exit (they cannot be reclassified as developed without harsher targets). The engine should compute tangled_rope with high variance across seats, reflecting this perspectival asymmetry. The claim and metrics are intentionally misaligned: claimed as tangled_rope (genuine coordination + asymmetric extraction); metrics authored as high extractiveness + substantial suppression + moderate theater—exactly the profile the misalignment captures.
 *
 * DIRECTIONALITY LOGIC:
 *   IPCC/UNFCCC authority: d ~0.0 (full beneficiary—the supranational reading is their authority structure operationalized; they control the empirical baseline and refuse to renegotiate it). Carbon-intensive nations: d ~0.95 (near-target; face ratcheting but retain some modulation via NDC submission; not fully trapped because coal→gas→renewables pathways exist, though costly). Fossil industries: d ~1.0 (full target; no exit except identity dissolution; regulatory extinction is embedded in the constraint's logic). Developing states with energy poverty: d ~0.75 (composite target/beneficiary; trapped in ratcheting requirements but nominally entitled to climate finance; the divergence between nominal benefit and structural extraction is the empirical content of the equity reading's objection). Clean energy industries: d ~0.05 (full beneficiary; structured into profitability; exit would mean markets reverting to carbon-intensity). Climate litigation coalition: d ~0.1 (beneficiary; gain institutional power and revenue from the constraint's justiciability). Equity advocates & sovereigntist governments: d ~0.5 (symmetric in principle—they mount resistance that the supranational reading must actively suppress, but they lack the capital or institutional leverage to exit). The high directionality variance (0.0–1.0 span) is the signature of a tangled-rope constraint: some seats gain from coordination (climate stability is a genuine collective good), others are pure extraction targets (fossil industries have no coordination interest).
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy risk is present but not yet realized. The founding problem (tragedy-of-the-commons carbon emissions requiring binding coordination) is live—climate outcomes worsen without coordination. However, the supranational reading's operationalization has created a secondary institutional mandate: the IPCC/UNFCCC/litigation infrastructure now generates its own constituency (clean-energy beneficiaries, institutional power, capital flows). If climate outcomes plateau or decouple from NDC commits (e.g., emissions trajectory unchanged despite NDC submission), the coordination function fails but the extraction and theatrical maintenance persist—tangled_rope → piton. Early signals: countries submitting NDCs known to be inadequate to 1.5°C (knowing they will miss targets), climate finance pledges consistently underdelivered (theater masking non-performance), and institutional expansion (more COP meetings, more IPCC cycles, more litigation) continuing regardless of emissions reduction progress. The measurement plateau (extractiveness & suppression flattening 25–40) is consistent with a constraint nearing piton transition if emissions outcomes decouple from institutional effort.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    empirical_binding_vs_formal_voluntariness,
    'Are NDCs binding international law (legally enforceable via supranational mechanisms), or voluntarily binding (states commit but retain exit power via non-ratification or withdrawal)?',
    'Adjudication by international courts (ICJ, regional courts) or major trade sanctions against NDC non-compliance. If courts recognize NDCs as justiciable and states face material consequences for non-compliance, the supranational reading is instantiated; if states successfully claim judicial immunity and trade sanctions do not materialize, the sovereigntist reading persists despite institutional layering.',
    'If binding: extractiveness remains high (~0.82), suppression logic is justified (binding requires active enforcement against non-compliance). If voluntarily binding: extractiveness drops to ~0.55, suppression becomes theatric (states perform compliance while retaining exit), constraint type shifts tangled_rope → rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(empirical_binding_vs_formal_voluntariness, conceptual, 'Whether NDCs are empirically binding or formally voluntary despite institutional operationalization.').

omega_variable(
    energy_poverty_exit_identity_lock,
    'Can developing states with energy poverty legitimately be reclassified as ''developed'' (thus escaping harsher targets), or is development-status classification itself weaponized to enforce ratcheting?',
    'Examine UNFCCC reclassification history: if reclassifications are granted on development metrics (GDP, HDI) independently of Paris Agreement compliance, the identity is not fully locked. If reclassifications are denied or withheld until NDC compliance is high, the classification is weaponized and the lock is real.',
    'If not weaponized: developing states have a structural exit (though costly—achieving developed-nation GDP per capita takes 30+ years). If weaponized: the identity lock is real, extractiveness vis-à-vis energy-poor states remains ~0.75, resistance from Global South increases, mandatrophy risk rises (institutional expansion without emissions progress).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(energy_poverty_exit_identity_lock, empirical, 'Whether the identity lock on developing states is structural or institutional.').

omega_variable(
    fossil_industry_adaptation_vs_extinction,
    'Can fossil-fuel industries adapt their business models (carbon capture, hydrogen, nuclear transition) while remaining structurally ''fossil fuel'' firms, or is regulatory extinction the only exit?',
    'Monitor major fossil-fuel firms'' capital allocation: if >50% of capex shifts to non-carbon energy over 10 years, adaptation is possible and the identity lock is permeable; if <30%, extinction is the only trajectory and the lock is real.',
    'If adaptation possible: some fossil firms exit via transformation, d drops from ~1.0 to ~0.6, extractiveness for that sub-population moderates. If extinction is only exit: the identity lock is real, d stays ~1.0, resistance to the supranational reading hardens, potential for regulatory capture and constraint modification increases.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(fossil_industry_adaptation_vs_extinction, empirical, 'Whether fossil-fuel industry identity lock is permeable or absolute.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the suppression of sovereigntist and equity alternatives structural (international institutions enforce the supranational reading and prevent alternatives from being operationalized) or internalized (states have accepted the supranational logic and suppress alternatives themselves)?',
    'Compare suppression intensity pre- and post-institutional capture. If suppression persists even after UNFCCC institutional authority is challenged or weakened, the suppression is internalized (states have internalized the 1.5°C framing). If suppression collapses when institutional authority wanes, it is structural.',
    'If structural: suppression can be reformed via institutional redesign (alternative authority structures could permit sovereigntist or equity readings). If internalized: structural suppression persists beyond institutional machinery (states continue enforcing even if UNFCCC is weakened), constraint type becomes more robust to authority challenges.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether suppression of alternative readings is maintained by external institutional pressure or internal state acceptance.').

omega_variable(
    climate_finance_contingency,
    'If climate finance and adaptation funding actually materialized at promised levels (scaled 4–10× current flows), would developing states transition from ''payers with energy poverty'' to genuine ''beneficiaries of ratcheting,'' or is the ratcheting itself incompatible with energy access?',
    'Model renewable energy system buildout pathways: if USD 500B annual climate finance (2025–2050) is sufficient to deliver both emissions reductions AND energy access in developing regions, the constraint is compatible with equity. If modeling shows capital requirements exceed plausible finance for that timeline, the incompatibility is structural.',
    'If compatible: the equity reading and supranational reading are reconcilable via better financing, beneficiary/victim structure shifts, and extractiveness for energy-poor states drops. If incompatible: energy-poor states are structurally locked as payers, equity reading forecloses the supranational reading (you cannot do both), mandatrophy risk rises (institutional growth without functional progress).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(climate_finance_contingency, empirical, 'Whether ratcheting and energy access are structurally compatible with realistic climate finance.').

omega_variable(
    kernel_contest_forecloses_vs_coexists,
    'Do the three readings of the Paris Article 4 NDC kernel (supranational, sovereigntist, equity) logically foreclose each other, or can they coexist as different parties'' live positions within a single institutional framework?',
    'Test coexistence: in UNFCCC consensus processes, can the three readings all remain live positions (each party''s negotiation stances reflect one reading, none is dismissed as illegitimate)? Or does supranational reading operationalization foreclose the others by making empirical claims (1.5°C) non-negotiable?',
    'If coexist: UNFCCC can accommodate all three, constraint type may soften (higher accessibility_collapse), institutional design is flexible. If supranational forecloses: UNFCCC is locked into one reading, equity and sovereigntist parties are structurally excluded (not just politically defeated), constraint type hardens, piton risk rises (institutional performance without genuine contestation).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_contest_forecloses_vs_coexists, conceptual, 'Whether the three readings of the Paris NDC kernel foreclose each other or remain live coexisting positions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(paris_article_4_ndc__supranational_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pari_tr_t0, paris_article_4_ndc__supranational_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(pari_tr_t0, observed).
narrative_ontology:measurement(pari_tr_t5, paris_article_4_ndc__supranational_reading, theater_ratio, 5, 0.21).
narrative_ontology:measurement_basis(pari_tr_t5, observed).
narrative_ontology:measurement(pari_tr_t10, paris_article_4_ndc__supranational_reading, theater_ratio, 10, 0.24).
narrative_ontology:measurement_basis(pari_tr_t10, observed).
narrative_ontology:measurement(pari_tr_t15, paris_article_4_ndc__supranational_reading, theater_ratio, 15, 0.26).
narrative_ontology:measurement_basis(pari_tr_t15, observed).
narrative_ontology:measurement(pari_tr_t20, paris_article_4_ndc__supranational_reading, theater_ratio, 20, 0.27).
narrative_ontology:measurement_basis(pari_tr_t20, observed).
narrative_ontology:measurement(pari_tr_t25, paris_article_4_ndc__supranational_reading, theater_ratio, 25, 0.28).
narrative_ontology:measurement_basis(pari_tr_t25, observed).
narrative_ontology:measurement(pari_tr_t30, paris_article_4_ndc__supranational_reading, theater_ratio, 30, 0.28).
narrative_ontology:measurement_basis(pari_tr_t30, observed).
narrative_ontology:measurement(pari_tr_t35, paris_article_4_ndc__supranational_reading, theater_ratio, 35, 0.28).
narrative_ontology:measurement_basis(pari_tr_t35, projected).
narrative_ontology:measurement(pari_tr_t40, paris_article_4_ndc__supranational_reading, theater_ratio, 40, 0.28).
narrative_ontology:measurement_basis(pari_tr_t40, projected).

% Extraction over time
narrative_ontology:measurement(pari_be_t0, paris_article_4_ndc__supranational_reading, base_extractiveness, 0, 0.62).
narrative_ontology:measurement_basis(pari_be_t0, observed).
narrative_ontology:measurement(pari_be_t5, paris_article_4_ndc__supranational_reading, base_extractiveness, 5, 0.67).
narrative_ontology:measurement_basis(pari_be_t5, observed).
narrative_ontology:measurement(pari_be_t10, paris_article_4_ndc__supranational_reading, base_extractiveness, 10, 0.72).
narrative_ontology:measurement_basis(pari_be_t10, observed).
narrative_ontology:measurement(pari_be_t15, paris_article_4_ndc__supranational_reading, base_extractiveness, 15, 0.76).
narrative_ontology:measurement_basis(pari_be_t15, observed).
narrative_ontology:measurement(pari_be_t20, paris_article_4_ndc__supranational_reading, base_extractiveness, 20, 0.79).
narrative_ontology:measurement_basis(pari_be_t20, observed).
narrative_ontology:measurement(pari_be_t25, paris_article_4_ndc__supranational_reading, base_extractiveness, 25, 0.81).
narrative_ontology:measurement_basis(pari_be_t25, observed).
narrative_ontology:measurement(pari_be_t30, paris_article_4_ndc__supranational_reading, base_extractiveness, 30, 0.82).
narrative_ontology:measurement_basis(pari_be_t30, observed).
narrative_ontology:measurement(pari_be_t35, paris_article_4_ndc__supranational_reading, base_extractiveness, 35, 0.82).
narrative_ontology:measurement_basis(pari_be_t35, projected).
narrative_ontology:measurement(pari_be_t40, paris_article_4_ndc__supranational_reading, base_extractiveness, 40, 0.82).
narrative_ontology:measurement_basis(pari_be_t40, projected).

% Suppression requirement over time
narrative_ontology:measurement(pari_su_t0, paris_article_4_ndc__supranational_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement_basis(pari_su_t0, observed).
narrative_ontology:measurement(pari_su_t5, paris_article_4_ndc__supranational_reading, suppression_requirement, 5, 0.54).
narrative_ontology:measurement_basis(pari_su_t5, observed).
narrative_ontology:measurement(pari_su_t10, paris_article_4_ndc__supranational_reading, suppression_requirement, 10, 0.61).
narrative_ontology:measurement_basis(pari_su_t10, observed).
narrative_ontology:measurement(pari_su_t15, paris_article_4_ndc__supranational_reading, suppression_requirement, 15, 0.66).
narrative_ontology:measurement_basis(pari_su_t15, observed).
narrative_ontology:measurement(pari_su_t20, paris_article_4_ndc__supranational_reading, suppression_requirement, 20, 0.69).
narrative_ontology:measurement_basis(pari_su_t20, observed).
narrative_ontology:measurement(pari_su_t25, paris_article_4_ndc__supranational_reading, suppression_requirement, 25, 0.71).
narrative_ontology:measurement_basis(pari_su_t25, observed).
narrative_ontology:measurement(pari_su_t30, paris_article_4_ndc__supranational_reading, suppression_requirement, 30, 0.71).
narrative_ontology:measurement_basis(pari_su_t30, observed).
narrative_ontology:measurement(pari_su_t35, paris_article_4_ndc__supranational_reading, suppression_requirement, 35, 0.71).
narrative_ontology:measurement_basis(pari_su_t35, projected).
narrative_ontology:measurement(pari_su_t40, paris_article_4_ndc__supranational_reading, suppression_requirement, 40, 0.71).
narrative_ontology:measurement_basis(pari_su_t40, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(paris_article_4_ndc__supranational_reading, global_infrastructure).
narrative_ontology:boltzmann_floor_override(paris_article_4_ndc__supranational_reading, 0.22).
narrative_ontology:affects_constraint(paris_article_4_ndc__supranational_reading, paris_article_4_ndc__sovereigntist_reading).
narrative_ontology:affects_constraint(paris_article_4_ndc__supranational_reading, paris_article_4_ndc__equity_reading).
narrative_ontology:affects_constraint(paris_article_4_ndc__supranational_reading, national_carbon_pricing_mechanisms).
narrative_ontology:affects_constraint(paris_article_4_ndc__supranational_reading, climate_litigation_justiciability).
narrative_ontology:affects_constraint(paris_article_4_ndc__supranational_reading, fossil_fuel_divestment_infrastructure).
narrative_ontology:affects_constraint(paris_article_4_ndc__supranational_reading, international_climate_finance).

% DUAL FORMULATION NOTE:
% The Paris Article 4 NDC kernel decomposes into three structurally distinct constraint stories, each instantiating a different reading of what NDCs bind and to whom. The supranational reading (this file) operationalizes NDCs as binding international commitments with empirical benchmarks (1.5°C) and transparency review. The sovereigntist reading (sibling file) interprets NDCs as voluntary self-determined pledges within a national energy sovereignty frame. The equity reading (sibling file) asserts NDCs must respect Common But Differentiated Responsibilities and asymmetric ratcheting. These are not different observables of one constraint; they are different constraints that share a kernel—each has its own epsilon (supranational: 0.82 extractiveness; sovereigntist: ~0.35; equity: contested between 0.45–0.70 depending on development status). The three readings are connected by network.affects_constraints in both directions: supranational reading influences the siblings structurally by making 1.5°C empirically non-negotiable; sovereigntist and equity readings influence supranational by contesting its institutional operationalization. The constraint family is linked via this network; they should be analyzed jointly to model how institutional layering (IPCC science + UNFCCC rules + litigation) forecloses or suppresses alternative readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(paris_article_4_ndc__supranational_reading, powerless, 0.82).
constraint_indexing:directionality_override(paris_article_4_ndc__supranational_reading, moderate, 0.71).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
