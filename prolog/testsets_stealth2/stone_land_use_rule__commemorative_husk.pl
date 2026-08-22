% ============================================================================
% CONSTRAINT STORY: stone_land_use_rule__commemorative_husk
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_stone_land_use_rule__commemorative_husk, []).

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
 *   constraint_id: stone_land_use_rule__commemorative_husk
 *   human_readable: Commemorative Husk of the Run-Up Stone: Warning Without Behavioral Force
 *   domain: disaster anthropology/institutional memory/land-use governance
 *
 * SUMMARY:
 *   A coastal town maintains a run-up stone inscribed by founders who
 *   survived an inundation: do not build your dwellings below this line.
 *   Under the reading instantiated here, the stone is a commemorative husk:
 *   it is cleaned, repainted, translated onto plaques, walked past by school
 *   groups, and featured in the annual rite — and it exerts zero force on
 *   land use. Permitting proceeds below the datum; waterfront inventory
 *   grows; the interpretive layer renders the imperative as sentiment. The
 *   arrangement's gains accrue to developers and the municipal fisc; its
 *   costs are deferred to future occupants and the national reconstruction
 *   fund. Claim and metrics are authored independently: the type claim is
 *   tangled_rope; the metrics describe the operation as measured. KEY AGENTS
 *   (by structural relationship): - coastal_municipal_government:
 *   Agenda-setter and beneficiary (institutional/arbitrage) — permits below
 *   the line and funds the rite - waterfront_developers: Primary beneficiary
 *   (powerful/arbitrage) — converts the dead prohibition into inventory -
 *   lowland_homeowners: Dual beneficiary/payer (moderate/constrained) —
 *   amenity now, tail later - future_lowland_residents: Primary target
 *   (powerless/trapped/generational) — bear the unwarned exposure -
 *   national_reconstruction_fund: Institutional payer (trapped) — absorbs the
 *   tail by statute - memorial_preservation_society: Beneficiary
 *   (organized/identity_locked) — paid in continuity for maintaining the husk
 *   - survivors_association: Excluded voice (powerless/identity_locked) —
 *   reads the stone as instruction, is heard as sentiment -
 *   prefectural_emergency_management_agency: Analytical observer
 *   (institutional/analytical) — holds the models that show the gap
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(stone_land_use_rule__commemorative_husk, 0.72).
domain_priors:suppression_score(stone_land_use_rule__commemorative_husk, 0.52).
domain_priors:theater_ratio(stone_land_use_rule__commemorative_husk, 0.78).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(stone_land_use_rule__commemorative_husk, extractiveness, 0.72).
narrative_ontology:constraint_metric(stone_land_use_rule__commemorative_husk, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(stone_land_use_rule__commemorative_husk, theater_ratio, 0.78).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(stone_land_use_rule__commemorative_husk, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(stone_land_use_rule__commemorative_husk, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(stone_land_use_rule__commemorative_husk, tangled_rope).
narrative_ontology:human_readable(stone_land_use_rule__commemorative_husk, "Commemorative Husk of the Run-Up Stone: Warning Without Behavioral Force").
narrative_ontology:topic_domain(stone_land_use_rule__commemorative_husk, "disaster anthropology/institutional memory/land-use governance").

domain_priors:requires_active_enforcement(stone_land_use_rule__commemorative_husk).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(stone_land_use_rule__commemorative_husk, '1744ef2c-6e3c-4738-9c79-e974b1add1f9').
narrative_ontology:cs_kernel_codification('1744ef2c-6e3c-4738-9c79-e974b1add1f9', fixed_text).
narrative_ontology:cs_authority_grounding('1744ef2c-6e3c-4738-9c79-e974b1add1f9', extraction).
narrative_ontology:cs_interpretation_layer_present('1744ef2c-6e3c-4738-9c79-e974b1add1f9').
narrative_ontology:cs_reading_relation('1744ef2c-6e3c-4738-9c79-e974b1add1f9', stone_land_use_rule__behavioral_competence, forecloses).
narrative_ontology:cs_axiom('1744ef2c-6e3c-4738-9c79-e974b1add1f9', foundational, commemoration_discharges_warning_duty).
narrative_ontology:cs_axiom_status(commemoration_discharges_warning_duty, holdable).
narrative_ontology:cs_axiom_grounding('1744ef2c-6e3c-4738-9c79-e974b1add1f9', commemoration_discharges_warning_duty, conventional).
narrative_ontology:cs_axiom('1744ef2c-6e3c-4738-9c79-e974b1add1f9', foundational, engineered_protection_supersedes_inscription).
narrative_ontology:cs_axiom_status(engineered_protection_supersedes_inscription, holdable).
narrative_ontology:cs_axiom_grounding('1744ef2c-6e3c-4738-9c79-e974b1add1f9', engineered_protection_supersedes_inscription, empirically_contingent).
narrative_ontology:cs_reference_frame('1744ef2c-6e3c-4738-9c79-e974b1add1f9', ancestral_memorial_non_binding).
narrative_ontology:cs_drift_state('1744ef2c-6e3c-4738-9c79-e974b1add1f9', contemporary_post_seawall_era, gap(repudiation_pressure, minor, true)).
narrative_ontology:cs_created_at('1744ef2c-6e3c-4738-9c79-e974b1add1f9', '').
narrative_ontology:cs_kernel_id(stone_land_use_rule__commemorative_husk, stone_land_use_rule).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(stone_land_use_rule__commemorative_husk, waterfront_developers).
narrative_ontology:constraint_beneficiary(stone_land_use_rule__commemorative_husk, coastal_municipal_government).
narrative_ontology:constraint_beneficiary(stone_land_use_rule__commemorative_husk, lowland_homeowners).
narrative_ontology:constraint_beneficiary(stone_land_use_rule__commemorative_husk, memorial_preservation_society).
narrative_ontology:constraint_victim(stone_land_use_rule__commemorative_husk, future_lowland_residents).
narrative_ontology:constraint_victim(stone_land_use_rule__commemorative_husk, national_reconstruction_fund).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(stone_land_use_rule__commemorative_husk, lowland_homeowners).
narrative_ontology:constraint_vindicates(stone_land_use_rule__commemorative_husk, commemoration_suffices_doctrine).
narrative_ontology:constraint_vindicates(stone_land_use_rule__commemorative_husk, engineered_protection_supersedes_inscription).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operates the permitting desk that approves construction below the inscribed high-water line, budgets the stone's annual cleaning and the anniversary rite, and promotes the waterfront's image. Collects tax base and civic standing from the growing below-line inventory; direct disaster losses are largely reimbursed by the national government, so the office's own ledger rarely records the tail. Staff rotate on career cycles; the office persists regardless of individual departures.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__commemorative_husk, coastal_municipal_government, agenda_setter,
    institutional, biographical, arbitrage, regional).
narrative_ontology:stakeholder_secondary_role(stone_land_use_rule__commemorative_husk, coastal_municipal_government, beneficiary).

% Assembles and builds parcels below the inscribed line, where zoning remains permissive because the marker carries no regulatory force. Sells view, access, and commute advantages; capital can be redeployed to other coasts before losses mature. Marketing materials cite the seawall and omit the published hazard maps.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__commemorative_husk, waterfront_developers, beneficiary,
    powerful, biographical, arbitrage, national).

% Owns amenity housing below the line — view, short commute, neighborhood continuity. Reads the maintained stone and the seawall together as evidence the hazard is handled. Equity is sunk in parcels that a revived prohibition would devalue; selling means accepting the discount they decline to price. Insurance is priced against averages rather than tails.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__commemorative_husk, lowland_homeowners, beneficiary,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(stone_land_use_rule__commemorative_husk, lowland_homeowners, payer).

% Will occupy the zone when run-up next exceeds the seawall. Purchases without the information the inscription was cut to transmit; holds no seat at hearings, no petition standing, no prior notice. Exposure is decided entirely in rooms they never enter, years before they arrive.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__commemorative_husk, future_lowland_residents, payer,
    powerless, generational, trapped, regional).

% Statutorily absorbs reconstruction and compensation costs when the event lands. Cannot decline the risk allocated to it; earlier reforms narrowed its levers over insurance mandates and transfer conditions. Prices the tail it is not positioned to prevent.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__commemorative_husk, national_reconstruction_fund, payer,
    institutional, generational, trapped, national).

% Cleans and repaints the inscription, translates it onto plaques, runs school visits and the annual rite. Draws continuity, local standing, and meaning from custody; ending the rite would dissolve the society's purpose. Members' identities are bound to the custodial role; stepping back feels like betraying the founders.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__commemorative_husk, memorial_preservation_society, beneficiary,
    organized, generational, identity_locked, local).

% Survivors and descendant families who read the inscription as instruction rather than ornament. Petitioned for refusal of below-line permits and for setbacks referenced to the stone's datum; appear in hearing minutes as objectors and nowhere in the permit file. Membership ages; each year fewer firsthand voices remain.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__commemorative_husk, survivors_association, excluded,
    powerless, generational, identity_locked, regional).

% Models inundation return periods, maintains the run-up catalog to which the stone's datum belongs, and publishes hazard maps that marketing materials omit. Advises the municipality but holds no permitting authority; observes the full distance between the maintained symbol and the built environment.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__commemorative_husk, prefectural_emergency_management_agency, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(stone_land_use_rule__commemorative_husk, waterfront_developers).
narrative_ontology:fixing_cost_class(stone_land_use_rule__commemorative_husk, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates collective remembrance: a fixed site and an annual rite where the community consolidates disaster memory, teaches children, and mourns. Historically the same stone also coordinated settlement placement away from the run-up zone — a function now dormant under this reading.
% TRANSFER_FUNCTION: Moves developable land value and municipal tax base from the hazard zone's future to the present: siting margin that the inscription was cut to protect is converted into current waterfront rents and convenience, with the tail cost deferred to future occupants and the national reconstruction fisc.
% ABSENT_VOICES: The survivors' association and descendant families would object to below-line permitting; they appear only as petitioners in hearing minutes. The class the stone was erected to protect — future occupants of the lowland — has no seat at all; their interests are voiced only retrospectively, after events.
% DISAPPEARANCE_RATIONALE: Land-use outcomes would barely move overnight — under this reading building decisions already ignore the stone — but the ritual calendar, school programming, and the civic story that the hazard is remembered would collapse. With the heritage counter-frame gone, survivors' petitions would lose their ceremonial answer and the line would become contestable again; fiscal flows tied to below-line assessment would persist because the building stock is sunk, so the rearrangement concentrates in memory practice and in the reopening of the zoning question.
% FOUNDING_PROBLEM: Recurrent inundation destroyed lowland settlements; the stone was raised to mark observed run-up height and to prohibit rebuilding below it, converting lived catastrophe into a durable siting rule.
% FOUNDING_PROBLEM_CORROBORATION: Attested from outside the benefiting set: the prefectural agency's run-up catalog and inundation models independently reproduce the stone's datum as consistent with historical events; parish and harbor records corroborate the founding inundations; survivor testimony collected by regional universities corroborates both the events and the original prohibition's wording. No benefiting party attests the founding problem — developer and municipal-fiscal materials describe the stone exclusively as heritage.
narrative_ontology:disappearance_verdict(stone_land_use_rule__commemorative_husk, world_rearranges).
narrative_ontology:founding_problem_status(stone_land_use_rule__commemorative_husk, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(stone_land_use_rule__commemorative_husk, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(stone_land_use_rule__commemorative_husk, 'none', 1).
narrative_ontology:epsilon_provenance(stone_land_use_rule__commemorative_husk, 0.72, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(stone_land_use_rule__commemorative_husk_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(stone_land_use_rule__commemorative_husk, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(stone_land_use_rule__commemorative_husk_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.72) because the standing arrangement converts a retired safety margin into present waterfront value while allocating the tail to unseated parties; the engine scales this by directionality and scope. Suppression (0.52) is raw and unscaled: no one coerces residents, but the revival alternative is actively resisted — heritage framing, permit-desk discretion, and lobbying keep the sibling reading off the books — and the risk-information channel is ritually neutralized. Theater_ratio (0.78) is the husk's signature: cleaning, repainting, plaques, school visits, and the annual rite consume most stone-related expenditure and produce no compliance. Accessibility_collapse is low (0.25): understanding the husk opens alternatives (build high, insure, rezone) rather than closing them. Resistance (0.35) reflects the survivors' association and occasional agency objections — real, aging, and outgunned. Time units are years since seawall commissioning; all three series share one grid (0-60 by tens) and end at the base_properties values. Suppression_requirement is tracked because the story's enforcement history is precisely the migration of enforcement effort from the rule to the husk's defense. Note the identity-coordination gaming risk: the declared coordination type accommodates genuine mnemonic complexity; it does not excuse the Power-by-Scope coupling visible here — institutional and powerful seats at regional scope collecting from a powerless, unseated class.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter and developer seats the arrangement presents as heritage stewardship plus ordinary land markets — nothing is being taken; the stone is honored and the market allocates. From the payer seats the same structure operates as a transfer: risk the stone was erected to prevent is converted into present rents and deferred onto occupants and the national fisc. The future-resident seat is computed entirely from declarations — no member of that class authored anything; their position is the story's starkest indexical fact. The observing agency sees both pictures and is structurally barred from acting on the second.
 *
 * DIRECTIONALITY LOGIC:
 *   Declarations map to directionality as follows. waterfront_developers (beneficiary, arbitrage-grade exit) derive nearest the beneficiary end — the arrangement subsidizes them by leaving inventory open. coastal_municipal_government (agenda_setter plus declared beneficiary) derives low d; the derivation is right — the office collects tax base and image value while the national backstop absorbs its tail. memorial_preservation_society (beneficiary, identity_locked) derives low d: custody pays them in continuity and standing. future_lowland_residents (payer, trapped, generational) derive near the full-target end — the arrangement's costs land on them with no exit and no seat. national_reconstruction_fund (payer, institutional, trapped) derives high d. One override: lowland_homeowners carry the moderate power atom, and the derivation reads their beneficiary declaration alone, which would understate their position; across biographical-plus horizons their net relationship is near-symmetric leaning target (amenity now, tail later, constrained exit), so d is overridden to 0.55. No override is placed on the institutional atom because it is shared by the municipality and the observing agency, and the derivation already separates them correctly through role declarations.
 *
 * MANDATROPHY ANALYSIS:
 *   The husk invites two mislabels. Read as pure piton — an inertial relic kept alive by ceremony — it would miss the concentrated capture: developers and the municipal fisc demonstrably profit from the prohibition's death, and a remnant with a concentrated capturer sits outside the piton cell. Read as pure snare — cover story plus victims — it would miss the genuine coordination the same structure performs: the memorial really does consolidate mourning, educate, and anchor civic identity, and that function is why the structure is defended by more than its profiteers. Tangled_rope holds both halves: coordinated through the same stone that extracts. Mandatrophy is declared resolved: the warning mandate outlived its function under this reading — the problem it served is live, but the arrangement abandoned serving it — which is exactly the seam the R5 interview records and the mismatch consumer cross-checks.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_divergence,
    'This story instantiates reading commemorative_husk of kernel stone_land_use_rule; the sibling reading behavioral_competence holds the same stone as a live prohibition enforced by daily practice. Which reading describes the stone''s actual present behavioral force?',
    'Permit-footprint audit: compare below-datum permitted floor area and transaction volumes against matched above-datum parcels across recent decades; statistical indistinguishability confirms the husk reading, systematic below-datum avoidance confirms the sibling.',
    'Confirmation of the sibling collapses this story''s extraction attribution (the arrangement becomes coordination with residual friction) and flips the family classification; confirmation of the husk fixes epsilon near its authored value.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_divergence, empirical, 'Which reading of the stone kernel describes present behavioral force.').

omega_variable(
    seawall_design_event_contingency,
    'Does the engineered seawall actually cover the relevant return-period event, or does it manufacture false security that substitutes for the retired warning?',
    'Probabilistic overtopping analysis against historical analog events and design-document review; natural experiment from any event that stresses the structure.',
    'If the seawall fails its design event, the husk reading''s obsolescence axiom loses its empirical ground, effective extraction rises sharply, and the arrangement trends toward pure extraction; if it performs, part of the measured extraction is better attributed to ordinary risk pricing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(seawall_design_event_contingency, empirical, 'Empirical adequacy of the engineered protection that replaced the inscription.').

omega_variable(
    interpretive_softening_intentionality,
    'Is the plaque program''s conversion of imperative inscriptions into sentimental dedications deliberate defanging by seated interests or sincere hermeneutic updating by custodians?',
    'Heritage-office deliberation archives, draft-translation comparisons across decades, and sponsorship records linking plaque funding to developer associations.',
    'Deliberate defanging attributes suppression to the agenda-setter seat and strengthens the capture reading of the husk; sincere updating keeps the decay inertial and the arrangement closer to a neglected remnant with incidental beneficiaries.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(interpretive_softening_intentionality, empirical, 'Intentionality behind the interpretive layer''s softening of the warning.').

omega_variable(
    founding_problem_framing_underdetermination,
    'Is the kernel''s founding problem ''keep settlement out of the run-up zone'' (which makes the husk a failed safety rule) or ''carry catastrophe memory forward'' (which makes the husk a functioning memorial whose land-use dimension was incidental)?',
    'Close reading of the founding inscriptions'' imperative grammar and of erection-era community records: did the founders authorize commemoration alone, or commemoration as carrier of a binding instruction?',
    'Under the memory-carrier framing the arrangement computes closer to pure coordination (genuine identity function, extraction incidental); under the siting-rule framing the authored tangled_rope classification stands with high extraction. The declared cs_structure values assume the siting-rule framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_framing_underdetermination, conceptual, 'Framing under-determination over what the stone was for.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(stone_land_use_rule__commemorative_husk, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ston_tr_t0, stone_land_use_rule__commemorative_husk, theater_ratio, 0, 0.35).
narrative_ontology:measurement_basis(ston_tr_t0, observed).
narrative_ontology:measurement(ston_tr_t10, stone_land_use_rule__commemorative_husk, theater_ratio, 10, 0.42).
narrative_ontology:measurement_basis(ston_tr_t10, observed).
narrative_ontology:measurement(ston_tr_t20, stone_land_use_rule__commemorative_husk, theater_ratio, 20, 0.5).
narrative_ontology:measurement_basis(ston_tr_t20, observed).
narrative_ontology:measurement(ston_tr_t30, stone_land_use_rule__commemorative_husk, theater_ratio, 30, 0.58).
narrative_ontology:measurement_basis(ston_tr_t30, observed).
narrative_ontology:measurement(ston_tr_t40, stone_land_use_rule__commemorative_husk, theater_ratio, 40, 0.66).
narrative_ontology:measurement_basis(ston_tr_t40, observed).
narrative_ontology:measurement(ston_tr_t50, stone_land_use_rule__commemorative_husk, theater_ratio, 50, 0.73).
narrative_ontology:measurement_basis(ston_tr_t50, observed).
narrative_ontology:measurement(ston_tr_t60, stone_land_use_rule__commemorative_husk, theater_ratio, 60, 0.78).
narrative_ontology:measurement_basis(ston_tr_t60, observed).

% Extraction over time
narrative_ontology:measurement(ston_be_t0, stone_land_use_rule__commemorative_husk, base_extractiveness, 0, 0.3).
narrative_ontology:measurement_basis(ston_be_t0, observed).
narrative_ontology:measurement(ston_be_t10, stone_land_use_rule__commemorative_husk, base_extractiveness, 10, 0.38).
narrative_ontology:measurement_basis(ston_be_t10, observed).
narrative_ontology:measurement(ston_be_t20, stone_land_use_rule__commemorative_husk, base_extractiveness, 20, 0.47).
narrative_ontology:measurement_basis(ston_be_t20, observed).
narrative_ontology:measurement(ston_be_t30, stone_land_use_rule__commemorative_husk, base_extractiveness, 30, 0.55).
narrative_ontology:measurement_basis(ston_be_t30, observed).
narrative_ontology:measurement(ston_be_t40, stone_land_use_rule__commemorative_husk, base_extractiveness, 40, 0.62).
narrative_ontology:measurement_basis(ston_be_t40, observed).
narrative_ontology:measurement(ston_be_t50, stone_land_use_rule__commemorative_husk, base_extractiveness, 50, 0.68).
narrative_ontology:measurement_basis(ston_be_t50, observed).
narrative_ontology:measurement(ston_be_t60, stone_land_use_rule__commemorative_husk, base_extractiveness, 60, 0.72).
narrative_ontology:measurement_basis(ston_be_t60, observed).

% Suppression requirement over time
narrative_ontology:measurement(ston_su_t0, stone_land_use_rule__commemorative_husk, suppression_requirement, 0, 0.25).
narrative_ontology:measurement_basis(ston_su_t0, observed).
narrative_ontology:measurement(ston_su_t10, stone_land_use_rule__commemorative_husk, suppression_requirement, 10, 0.28).
narrative_ontology:measurement_basis(ston_su_t10, observed).
narrative_ontology:measurement(ston_su_t20, stone_land_use_rule__commemorative_husk, suppression_requirement, 20, 0.33).
narrative_ontology:measurement_basis(ston_su_t20, observed).
narrative_ontology:measurement(ston_su_t30, stone_land_use_rule__commemorative_husk, suppression_requirement, 30, 0.4).
narrative_ontology:measurement_basis(ston_su_t30, observed).
narrative_ontology:measurement(ston_su_t40, stone_land_use_rule__commemorative_husk, suppression_requirement, 40, 0.46).
narrative_ontology:measurement_basis(ston_su_t40, observed).
narrative_ontology:measurement(ston_su_t50, stone_land_use_rule__commemorative_husk, suppression_requirement, 50, 0.5).
narrative_ontology:measurement_basis(ston_su_t50, observed).
narrative_ontology:measurement(ston_su_t60, stone_land_use_rule__commemorative_husk, suppression_requirement, 60, 0.52).
narrative_ontology:measurement_basis(ston_su_t60, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(stone_land_use_rule__commemorative_husk, identity_coordination).
narrative_ontology:affects_constraint(stone_land_use_rule__commemorative_husk, stone_land_use_rule__behavioral_competence).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of kernel stone_land_use_rule per the epsilon-invariance principle: the colloquial label 'the stone's rule' conflates two structurally distinct claims — (a) the stone presently functions as a live land-use prohibition (sibling: stone_land_use_rule__behavioral_competence), and (b) the stone functions as a maintained memorial with zero behavioral force (this file: commemorative_husk). The readings assign different epsilon over the same artifact: the sibling's arrangement carries coordination friction with negligible extraction; this reading's arrangement carries high extraction via the vacated safety margin. The sibling reading is upstream in legitimacy (founders' intent, inscription grammar); this reading feeds on its decay. This file links the sibling via affects_constraints; the sibling file reciprocates.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(stone_land_use_rule__commemorative_husk, moderate, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
