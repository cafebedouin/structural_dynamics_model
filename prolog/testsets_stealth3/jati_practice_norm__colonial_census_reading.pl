% ============================================================================
% CONSTRAINT STORY: jati_practice_norm__colonial_census_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jati_practice_norm__colonial_census_reading, []).

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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: jati_practice_norm__colonial_census_reading
 *   human_readable: Colonial Census Stabilization of Jati Categories (1871-1941)
 *   domain: social anthropology/political economy/religious studies
 *
 * SUMMARY:
 *   From 1871 the decennial all-India census translated locally negotiated,
 *   seasonally shifting jati affiliations into fixed administrative
 *   categories: standardized names, printed rank orders of precedence,
 *   ethnographic glossaries, and adjudicated reclassification petitions. The
 *   stated purpose was governance legibility — revenue, recruitment, famine
 *   relief, and later legislative seat calculations all ran through the
 *   printed tables. The effect, on this reading, was to freeze a fluid status
 *   order into an official taxonomy that communities then had to live inside:
 *   petitioning replaced bargaining, and the printed entry became the fact
 *   that local life had to accommodate. This file instantiates ONE reading of
 *   the kernel jati_practice_norm — the colonial_census_reading. The
 *   orthodox_textual_reading and localized_practice_reading are separate
 *   constraints in separate files; their contest is routed to omega
 *   variables, not folded into this epsilon. The epsilon referent here is the
 *   standing arrangement under contest — the census-stabilized category
 *   regime — assessed by this reading's own lights, never the practice-based
 *   arrangement the localized reading would endorse. The claimed type
 *   (tangled_rope) and the authored metrics are independent facts: the claim
 *   states what this reading believes is structurally true; the metrics state
 *   what is descriptively true of the apparatus's operation.
 *
 * KEY AGENTS:
 *   - colonial_administration: Primary agenda-setter and receipt-seat (institutional/arbitrage) — designs schedules, fixes category lists, adjudicates petitions; collects legibility, revenue, and recruitment control; can reshape or drop the exercise from the metropolitan office
 *   - imperial_ethnographers: Secondary beneficiary (institutional/mobile) — compile glossaries, rank orders, and anthropometric surveys; convert classification work into careers and metropolitan authority
 *   - martial_race_designated_communities: Situated beneficiary (organized/constrained) — recruitment preference, pensions, and official esteem flow from a favorable entry that simultaneously binds them to recruitment expectations and flattens internal difference
 *   - missionary_societies: Incidental beneficiary (institutional/mobile) — consume the published tables to target schooling and conversion work without operating any part of the counting machinery
 *   - indigenous_elite_intermediaries: Local administrator-beneficiary (moderate/constrained) — fill returns, translate answers into schedule categories, convert classification access into patronage
 *   - village_level_status_negotiators: Primary target (moderate/trapped) — locally bargained standing is written down once and defended thereafter by officials citing the printed table
 *   - low_classification_petitioner_groups: Primary target (moderate/trapped) — memorial campaigns and scribes against a printed entry that schools, recruiters, and courts treat as fact
 *   - ambiguous_identity_holders: Sharpest target (powerless/trapped) — mixed descent and dual occupations forced into single boxes they lack the documentation to contest
 *   - women_in_enumerated_households: Excluded voice (powerless/trapped) — returns authored by male household heads; their own accounts never enter the record directly
 *   - nomadic_trading_bands: Excluded voice (powerless/trapped) — mobile livelihoods illegible to the sedentary frame; their entries feed policing lists more than services
 *   - postcolonial_census_historians: Analytical observer (analytical/analytical) — reconstruct design choices from archives and trace how printed categories reshaped subsequent politics
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jati_practice_norm__colonial_census_reading, 0.66).
domain_priors:suppression_score(jati_practice_norm__colonial_census_reading, 0.44).
domain_priors:theater_ratio(jati_practice_norm__colonial_census_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jati_practice_norm__colonial_census_reading, extractiveness, 0.66).
narrative_ontology:constraint_metric(jati_practice_norm__colonial_census_reading, suppression_requirement, 0.44).
narrative_ontology:constraint_metric(jati_practice_norm__colonial_census_reading, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jati_practice_norm__colonial_census_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(jati_practice_norm__colonial_census_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jati_practice_norm__colonial_census_reading, tangled_rope).
narrative_ontology:human_readable(jati_practice_norm__colonial_census_reading, "Colonial Census Stabilization of Jati Categories (1871-1941)").
narrative_ontology:topic_domain(jati_practice_norm__colonial_census_reading, "social anthropology/political economy/religious studies").

domain_priors:requires_active_enforcement(jati_practice_norm__colonial_census_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jati_practice_norm__colonial_census_reading, '59d67ab6-ac93-4fee-b9a0-4fe765757f4f').
narrative_ontology:cs_kernel_codification('59d67ab6-ac93-4fee-b9a0-4fe765757f4f', formalized).
narrative_ontology:cs_authority_grounding('59d67ab6-ac93-4fee-b9a0-4fe765757f4f', extraction).
narrative_ontology:cs_interpretation_layer_present('59d67ab6-ac93-4fee-b9a0-4fe765757f4f').
narrative_ontology:cs_reading_relation('59d67ab6-ac93-4fee-b9a0-4fe765757f4f', jati_practice_norm__orthodox_textual_reading, influences).
narrative_ontology:cs_reading_relation('59d67ab6-ac93-4fee-b9a0-4fe765757f4f', jati_practice_norm__localized_practice_reading, influences).
narrative_ontology:cs_axiom('59d67ab6-ac93-4fee-b9a0-4fe765757f4f', foundational, jati_is_enumerable_administrative_fact).
narrative_ontology:cs_axiom_status(jati_is_enumerable_administrative_fact, holdable).
narrative_ontology:cs_axiom_grounding('59d67ab6-ac93-4fee-b9a0-4fe765757f4f', jati_is_enumerable_administrative_fact, empirically_contingent).
narrative_ontology:cs_axiom('59d67ab6-ac93-4fee-b9a0-4fe765757f4f', secondary, classification_precedes_governance).
narrative_ontology:cs_axiom_status(classification_precedes_governance, holdable).
narrative_ontology:cs_axiom_grounding('59d67ab6-ac93-4fee-b9a0-4fe765757f4f', classification_precedes_governance, instrumental).
narrative_ontology:cs_reference_frame('59d67ab6-ac93-4fee-b9a0-4fe765757f4f', enumerable_hierarchical_jati_taxonomy).
narrative_ontology:cs_drift_state('59d67ab6-ac93-4fee-b9a0-4fe765757f4f', late_colonial_period, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('59d67ab6-ac93-4fee-b9a0-4fe765757f4f', '').
narrative_ontology:cs_kernel_id(jati_practice_norm__colonial_census_reading, jati_practice_norm).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jati_practice_norm__colonial_census_reading, colonial_administration).
narrative_ontology:constraint_beneficiary(jati_practice_norm__colonial_census_reading, imperial_ethnographers).
narrative_ontology:constraint_beneficiary(jati_practice_norm__colonial_census_reading, missionary_societies).
narrative_ontology:constraint_beneficiary(jati_practice_norm__colonial_census_reading, martial_race_designated_communities).
narrative_ontology:constraint_victim(jati_practice_norm__colonial_census_reading, village_level_status_negotiators).
narrative_ontology:constraint_victim(jati_practice_norm__colonial_census_reading, low_classification_petitioner_groups).
narrative_ontology:constraint_victim(jati_practice_norm__colonial_census_reading, ambiguous_identity_holders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(jati_practice_norm__colonial_census_reading, indigenous_elite_intermediaries).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Designs the census schedules, fixes the category lists, adjudicates reclassification petitions, and publishes the decennial tables. Revenue assessment, army recruitment, famine relief targeting, and legislative seat calculations all run through its categories. Its officers rotate out on pension and the metropolitan office can reshape or drop the exercise at will.
narrative_ontology:constraint_stakeholder(jati_practice_norm__colonial_census_reading, colonial_administration, agenda_setter,
    institutional, generational, arbitrage, continental).

% Compile the ethnographic glossaries, rank orders of precedence, and anthropometric surveys that dress the counts in scholarship. Careers, fellowships, and metropolitan reputations are built on the classification enterprise; individuals move between census offices, universities, and museum posts.
narrative_ontology:constraint_stakeholder(jati_practice_norm__colonial_census_reading, imperial_ethnographers, beneficiary,
    institutional, generational, mobile, continental).

% Use the published tables to locate populations by denomination-relevant category and target schooling, medical missions, and conversion work. They collect ready-made maps of souls without operating any part of the counting machinery.
narrative_ontology:constraint_stakeholder(jati_practice_norm__colonial_census_reading, missionary_societies, beneficiary,
    institutional, generational, mobile, continental).

% Communities whose census entry carries a recruitment-favorable designation receive army enlistment preference, pensions, and official esteem. The same entry binds them to recruitment expectations, flattens internal differences into one official profile, and makes any later bid to revise the entry a fight against their own recorded past.
narrative_ontology:constraint_stakeholder(jati_practice_norm__colonial_census_reading, martial_race_designated_communities, beneficiary,
    organized, generational, constrained, regional).

% Literate local notables hired as enumerators and record-keepers fill in household returns, translate answers into schedule categories, and advise neighbors on petition wording. The post pays modestly but converts classification access into lasting patronage, and their own communities' entries tend to fare well.
narrative_ontology:constraint_stakeholder(jati_practice_norm__colonial_census_reading, indigenous_elite_intermediaries, agenda_setter,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(jati_practice_norm__colonial_census_reading, indigenous_elite_intermediaries, beneficiary).

% Village communities whose standing was settled episode by episode through marriage alliances, occupation shifts, and council bargaining find their position written down once and defended thereafter by officials citing the printed table. Renegotiation now requires petitioning a distant office in writing; the old local channels persist socially but carry no administrative weight.
narrative_ontology:constraint_stakeholder(jati_practice_norm__colonial_census_reading, village_level_status_negotiators, payer,
    moderate, biographical, trapped, regional).

% Groups entered in subordinate rows organize memorials, hire scribes, and lobby visiting officers for reclassification. Successes are rare and slow; meanwhile schools, recruiting officers, and courts treat the printed entry as fact. Withdrawal from the register is not offered as an option.
narrative_ontology:constraint_stakeholder(jati_practice_norm__colonial_census_reading, low_classification_petitioner_groups, payer,
    moderate, biographical, trapped, regional).

% Households with mixed descent, recent migration, or two seasonal occupations must be entered somewhere. The enumerator picks one box; correction requires documentation most lack. Kin networks straddling category lines get split across the record.
narrative_ontology:constraint_stakeholder(jati_practice_norm__colonial_census_reading, ambiguous_identity_holders, payer,
    powerless, biographical, trapped, local).

% Returns are authored by male household heads; women's own accounts of household composition, work, and affiliation enter the record only as reported by another. Asked directly, they would describe households, and themselves, differently.
narrative_ontology:constraint_stakeholder(jati_practice_norm__colonial_census_reading, women_in_enumerated_households, excluded,
    powerless, biographical, trapped, local).

% Pastoral and itinerant trading groups move with the seasons; the sedentary village frame catches them badly or misses them, and their entries feed policing lists more than services. Asked to design a record of themselves, they would describe mobility, not residence.
narrative_ontology:constraint_stakeholder(jati_practice_norm__colonial_census_reading, nomadic_trading_bands, excluded,
    powerless, immediate, trapped, regional).

% Reconstruct the census's design choices from archives, compare enumeration regimes across empires, and trace how the printed categories reshaped the politics that followed. They hold no stake in any entry and can see the whole apparatus at once.
narrative_ontology:constraint_stakeholder(jati_practice_norm__colonial_census_reading, postcolonial_census_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jati_practice_norm__colonial_census_reading, colonial_administration).
narrative_ontology:fixing_cost_class(jati_practice_norm__colonial_census_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Made a vast, internally heterogeneous population administratively legible: standardized names, counts, and territorial rolls that let a subcontinent-scale polity run taxation, army recruitment, famine relief, epidemic response, and legislative seat calculation through a single set of categories.
% TRANSFER_FUNCTION: Moves status-determination authority from local communities to the central administrative apparatus; moves classification rents — recruitment preference, political weight, patronage over petition outcomes — toward groups favored in the fixed order; moves the costs of administrative convenience onto communities whose lived boundaries did not match the printed boxes.
% ABSENT_VOICES: Women within enumerated households (returns authored by male heads) would object that the record describes someone else's account of them; nomadic and pastoral bands would object that the sedentary frame renders their livelihoods illegible and feeds policing rather than services; ordinary respondents unable to read their own entries would object that correction requires documentation they were never given. None of these seats was in the room where schedules were designed.
% DISAPPEARANCE_RATIONALE: If the apparatus vanished overnight, revenue and recruitment systems would lose their category index and have to rebuild legibility by other means; the petition economy, the intermediary patronage networks, and the communal-electoral blocs organized around printed entries would lose their currency; and community self-understanding, deprived of the official mirror, would remain locally negotiated for longer. Arrangements across administration and society depend on it.
% FOUNDING_PROBLEM: Governing a subcontinent-sized population with pre-modern information infrastructure: the Company state and its successor needed counts, names, and stable categories to tax, conscript, vaccinate, and police at scale.
% FOUNDING_PROBLEM_CORROBORATION: Attested from outside the benefiting parties by anti-colonial legislators and nationalist statisticians who contested the exercise's premises in print during the interval, and by independent demographic historians reconstructing the archive, who attest the schedules were designed for imperial revenue, recruitment, and police indexing rather than for any community's own purposes. Census commissioners' private papers concede instrumental purposes their public reports soften — admissions from inside the beneficiary set, usable only as corroboration-that-even-insiders-knew. No attestation from outside the benefiting parties supports a community-serving founding purpose.
narrative_ontology:disappearance_verdict(jati_practice_norm__colonial_census_reading, world_rearranges).
narrative_ontology:founding_problem_status(jati_practice_norm__colonial_census_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jati_practice_norm__colonial_census_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(jati_practice_norm__colonial_census_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jati_practice_norm__colonial_census_reading, 0.66, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jati_practice_norm__colonial_census_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jati_practice_norm__colonial_census_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jati_practice_norm__colonial_census_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.66 at interval end) is moderate-high: the apparatus transfers status-determination authority from communities to the center, imposes misclassification costs on those whose lives do not fit the boxes, and by the 1930s the categories have become the currency of communal-electoral politics, locking extraction in. Suppression (0.44 end-state) follows a rise-and-fall arc rather than a ratchet: enforcement machinery built up from the crude first count through the disciplined schedules and classification committees of 1891-1901 (peak 0.62), then decayed as the government abandoned rank-ordering after 1911 and publicly doubted the exercise by 1931 — the series models that enforcement-capacity history specifically. Theater (0.52) crosses the proxy-substitution line by interval end: the ethnographic superstructure (anthropometric surveys, precedence lists, gazetteers) increasingly performed scientific objectivity while actual governance decisions ran on crude pre-fixed stereotypes; the 1941 wartime count was skeletal, and much remaining activity defended the frame rather than informed it. Accessibility_collapse (0.62) reflects a distinctive shape: alternatives did not disappear so much as collapse INTO the frame — local renegotiation channels lost administrative force, and the only remaining route (petition) runs through the apparatus itself. Resistance (0.55) is substantial and documented: early census boycotts, fabricated and evasive returns, mass memorial campaigns, and nationalist critique of enumeration itself. All three series share one eight-point grid aligned to the actual decennial census years, so every metric is authored at every examined time point. Coordination type is declared identity_coordination: the apparatus's primary function is adjudicating group-membership claims against fixed criteria — allocation of recruitment and representation rides on that membership stabilization, not the reverse.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat experiences the apparatus as a technical instrument it can redesign at will — its officers rotate out on pension and the metropolitan office holds arbitrage-grade exit, so from that seat the categories are provisional tools. The payer seats experience a one-way inscription: the printed entry precedes them, outlasts them, and cannot be exited, only petitioned. The situated beneficiary seat (martial-race-designated communities) experiences advantage as obligation — the favorable entry delivers recruitment preference while binding the community to recruitment expectations and erasing internal difference. Among same-power actors, village negotiators and petitioner groups both hold moderate power, but constraint-specific assets differentiate them: literacy, scribal access, and urban ties determine whose petitions even reach an officer, so identical nominal standing yields different effective positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations drive the derivation: the administration, ethnographers, and missionaries sit near the beneficiary end (d low); the three victim classes sit near the full-target end, sharpened by trapped exit — the sharper the trap, the nearer d approaches 1.0, with ambiguous_identity_holders (powerless, local, undocumented) at the extreme. One override is declared: the derivation from the beneficiary declaration plus constrained exit would place the organized seat (martial-race-designated communities) deep on the beneficiary side (d near 0.1), but their binding into recruitment obligations, casualty-bearing, and loss of internal autonomy put them materially nearer symmetric (d 0.32). The override corrects a derivation that reads the favorable entry while missing what the entry costs its holders. Suppression is authored as a raw structural property and is NOT scaled by power or scope; only extractiveness is scaled, by directionality and the continental scope of the apparatus.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope framing is what keeps this analysis honest in both directions. A pure-extraction reading would erase the real coordination function: taxation, famine relief targeting, vaccination logistics, and army recruitment genuinely ran through the categories, and a subcontinent-scale polity did need legibility it had no other way to obtain. A pure-coordination reading would erase the transfer: the same tables that moved grain also moved status-determination authority from village councils to printing offices, and the rigidity was partly manufactured, not merely recorded. On the genealogy question, the founding problem — imperial administrative legibility — is dead with the colonial state, yet the arrangement persists in successor forms; the status-dead-plus-world-rearranges mismatch is the zombie flag, and the classification apparatus is what surfaces it rather than letting the inherited categories pass as a neutral coordination tool.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_attribution,
    'Does the measured extraction belong to the census apparatus itself or to the underlying jati practice norm it froze — i.e., would the localized_practice_reading attribute the same costs to the norm rather than to the external frame?',
    'Author the sibling readings over the shared referent and compare epsilon; examine regions and decades where census penetration was thin to see whether fluidity and its costs persisted without the apparatus.',
    'If extraction tracks the apparatus rather than the norm, remediation targets the administrative frame; if the norm itself carried comparable costs, the sibling reading''s implied lower-epsilon claim fails and the readings converge.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_attribution, conceptual, 'Attribution of extraction between the external apparatus and the underlying practice norm across kernel readings.').

omega_variable(
    prior_fluidity_baseline,
    'Were jati boundaries substantially fluid before 1871, or was the census recording an already-hardening system?',
    'Code pre-census vernacular records, local court documents, mission linguistics, and travelogues for boundary-crossing frequency (marriage, occupation shift, affiliation change) by region and decade.',
    'A fluid baseline sustains the high-epsilon attribution to the census; a rigid baseline shifts extraction backward to earlier consolidations and lowers this constraint''s epsilon.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(prior_fluidity_baseline, empirical, 'Whether the census froze fluid categories or recorded already-frozen ones.').

omega_variable(
    endogenous_consolidation_loop,
    'Are post-census rigidities endogenous to the census incentive structure — communities consolidating identities to compete inside the frame — such that removing the apparatus would not restore fluidity?',
    'Compare identity-consolidation rates in high- versus low-enumeration regions and periods; track whether petition-driven consolidation persists after administrative demand for the categories lapses.',
    'If endogenous, the stabilized categories have become load-bearing and removal produces rearrangement rather than return, raising the transition-question salience; if exogenous, the frame is removable overhead.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(endogenous_consolidation_loop, empirical, 'Reversibility of category rigidification once census incentives lapse.').

omega_variable(
    submerged_practice_persistence,
    'How much continuous local renegotiation persisted beneath the official frame, and does the authored suppression overstate the displacement of the localized_practice_reading''s alternative?',
    'Micro-historical comparison of actual marriage, occupation, and affiliation patterns against census returns for the same villages and decades.',
    'Substantial persistence lowers effective suppression and accessibility_collapse, moving the computed type toward a lighter hybrid; negligible persistence confirms displacement of the alternative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(submerged_practice_persistence, empirical, 'Persistence of the submerged local-renegotiation alternative beneath the administrative frame.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jati_practice_norm__colonial_census_reading, 1871, 1941).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jati_tr_t1871, jati_practice_norm__colonial_census_reading, theater_ratio, 1871, 0.12).
narrative_ontology:measurement(jati_tr_t1881, jati_practice_norm__colonial_census_reading, theater_ratio, 1881, 0.18).
narrative_ontology:measurement(jati_tr_t1891, jati_practice_norm__colonial_census_reading, theater_ratio, 1891, 0.3).
narrative_ontology:measurement(jati_tr_t1901, jati_practice_norm__colonial_census_reading, theater_ratio, 1901, 0.42).
narrative_ontology:measurement(jati_tr_t1911, jati_practice_norm__colonial_census_reading, theater_ratio, 1911, 0.45).
narrative_ontology:measurement(jati_tr_t1921, jati_practice_norm__colonial_census_reading, theater_ratio, 1921, 0.44).
narrative_ontology:measurement(jati_tr_t1931, jati_practice_norm__colonial_census_reading, theater_ratio, 1931, 0.47).
narrative_ontology:measurement(jati_tr_t1941, jati_practice_norm__colonial_census_reading, theater_ratio, 1941, 0.52).

% Extraction over time
narrative_ontology:measurement(jati_be_t1871, jati_practice_norm__colonial_census_reading, base_extractiveness, 1871, 0.3).
narrative_ontology:measurement(jati_be_t1881, jati_practice_norm__colonial_census_reading, base_extractiveness, 1881, 0.37).
narrative_ontology:measurement(jati_be_t1891, jati_practice_norm__colonial_census_reading, base_extractiveness, 1891, 0.46).
narrative_ontology:measurement(jati_be_t1901, jati_practice_norm__colonial_census_reading, base_extractiveness, 1901, 0.56).
narrative_ontology:measurement(jati_be_t1911, jati_practice_norm__colonial_census_reading, base_extractiveness, 1911, 0.59).
narrative_ontology:measurement(jati_be_t1921, jati_practice_norm__colonial_census_reading, base_extractiveness, 1921, 0.61).
narrative_ontology:measurement(jati_be_t1931, jati_practice_norm__colonial_census_reading, base_extractiveness, 1931, 0.64).
narrative_ontology:measurement(jati_be_t1941, jati_practice_norm__colonial_census_reading, base_extractiveness, 1941, 0.66).

% Suppression requirement over time
narrative_ontology:measurement(jati_su_t1871, jati_practice_norm__colonial_census_reading, suppression_requirement, 1871, 0.35).
narrative_ontology:measurement(jati_su_t1881, jati_practice_norm__colonial_census_reading, suppression_requirement, 1881, 0.42).
narrative_ontology:measurement(jati_su_t1891, jati_practice_norm__colonial_census_reading, suppression_requirement, 1891, 0.55).
narrative_ontology:measurement(jati_su_t1901, jati_practice_norm__colonial_census_reading, suppression_requirement, 1901, 0.62).
narrative_ontology:measurement(jati_su_t1911, jati_practice_norm__colonial_census_reading, suppression_requirement, 1911, 0.55).
narrative_ontology:measurement(jati_su_t1921, jati_practice_norm__colonial_census_reading, suppression_requirement, 1921, 0.5).
narrative_ontology:measurement(jati_su_t1931, jati_practice_norm__colonial_census_reading, suppression_requirement, 1931, 0.48).
narrative_ontology:measurement(jati_su_t1941, jati_practice_norm__colonial_census_reading, suppression_requirement, 1941, 0.44).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jati_practice_norm__colonial_census_reading, identity_coordination).
narrative_ontology:affects_constraint(jati_practice_norm__colonial_census_reading, orthodox_textual_reading).
narrative_ontology:affects_constraint(jati_practice_norm__colonial_census_reading, localized_practice_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'caste/jati' conflates three structurally distinct constraints with different epsilon values: the textual-varna orthodoxy claim, the living local-renegotiation norm, and the externally imposed census fixation. Each is authored as its own story with its own beneficiaries, victims, and classification; this file authors the third. The census reading sits downstream of the textual reading (schedules and rank orders imported varna-ordering assumptions into administration) and upstream of modern enumerated-identity politics; the affects_constraints edges connect the three family members so contamination and decomposition analysis can traverse them.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(jati_practice_norm__colonial_census_reading, organized, 0.32).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
