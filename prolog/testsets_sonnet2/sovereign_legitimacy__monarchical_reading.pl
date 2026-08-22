% ============================================================================
% CONSTRAINT STORY: sovereign_legitimacy__monarchical_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sovereign_legitimacy__monarchical_reading, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: sovereign_legitimacy__monarchical_reading
 *   human_readable: Monarchical Reading of Sovereign Legitimacy (Divine Right / Hereditary Succession)
 *   domain: political_philosophy/constitutional_theory
 *
 * SUMMARY:
 *   This constraint instantiates the monarchical reading of the contested
 *   sovereign_legitimacy kernel: authority flows downward from the sovereign
 *   through inherited right, grounded in divine sanction, tradition, and
 *   bloodline continuity. This is one of three structurally distinct claims
 *   that share the label 'legitimate sovereign authority' — the others being
 *   the republican reading (authority flows upward through consent) and the
 *   constitutional hybrid reading (dual-sourced ceremonial/political
 *   authority). Each reading has its own beneficiary/victim structure, its
 *   own ε, and its own classification; they are not the same constraint
 *   viewed from different angles. This file authors only the monarchical
 *   reading, as the reading's own lights would assess the standing
 *   arrangement — a bloodline-and-divine-sanction hierarchy under contest by
 *   rival legitimacy claims.
 *
 * KEY AGENTS:
 *   - hereditary_ruling_dynasty: Primary agenda-setter and beneficiary (institutional/arbitrage) — administers succession law, collects the yield of the arrangement
 *   - landed_aristocracy: Secondary beneficiary (powerful/constrained) — legitimacy contingent on the same bloodline logic
 *   - established_church_hierarchy: Co-agenda-setter (institutional/arbitrage) — supplies doctrinal certification of divine sanction
 *   - commoner_subjects: Primary target (powerless/trapped) — bears the extraction with no participatory channel
 *   - excluded_collateral_bloodlines: Secondary target (moderate/constrained) — structurally foreclosed despite plausible claims
 *   - urban_merchant_class: Secondary target (moderate/constrained) — economic power without political standing
 *   - constitutional_theorists: Analytical observer — compares readings across the kernel
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sovereign_legitimacy__monarchical_reading, 0.78).
domain_priors:suppression_score(sovereign_legitimacy__monarchical_reading, 0.87).
domain_priors:theater_ratio(sovereign_legitimacy__monarchical_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sovereign_legitimacy__monarchical_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(sovereign_legitimacy__monarchical_reading, suppression_requirement, 0.87).
narrative_ontology:constraint_metric(sovereign_legitimacy__monarchical_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sovereign_legitimacy__monarchical_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(sovereign_legitimacy__monarchical_reading, resistance, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sovereign_legitimacy__monarchical_reading, tangled_rope).
narrative_ontology:human_readable(sovereign_legitimacy__monarchical_reading, "Monarchical Reading of Sovereign Legitimacy (Divine Right / Hereditary Succession)").
narrative_ontology:topic_domain(sovereign_legitimacy__monarchical_reading, "political_philosophy/constitutional_theory").

domain_priors:requires_active_enforcement(sovereign_legitimacy__monarchical_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sovereign_legitimacy__monarchical_reading, 'fcf62519-a854-46f2-9a67-c6eeba42ec76').
narrative_ontology:cs_kernel_codification('fcf62519-a854-46f2-9a67-c6eeba42ec76', distributed).
narrative_ontology:cs_authority_grounding('fcf62519-a854-46f2-9a67-c6eeba42ec76', lineage).
narrative_ontology:cs_interpretation_layer_present('fcf62519-a854-46f2-9a67-c6eeba42ec76').
narrative_ontology:cs_reading_relation('fcf62519-a854-46f2-9a67-c6eeba42ec76', sovereign_legitimacy__republican_reading, forecloses).
narrative_ontology:cs_reading_relation('fcf62519-a854-46f2-9a67-c6eeba42ec76', sovereign_legitimacy__constitutional_hybrid_reading, influences).
narrative_ontology:cs_axiom('fcf62519-a854-46f2-9a67-c6eeba42ec76', foundational, authority_descends_from_sovereign_by_inherited_right).
narrative_ontology:cs_axiom_status(authority_descends_from_sovereign_by_inherited_right, holdable).
narrative_ontology:cs_axiom_grounding('fcf62519-a854-46f2-9a67-c6eeba42ec76', authority_descends_from_sovereign_by_inherited_right, theological).
narrative_ontology:cs_axiom('fcf62519-a854-46f2-9a67-c6eeba42ec76', foundational, bloodline_continuity_constitutes_legitimacy).
narrative_ontology:cs_axiom_status(bloodline_continuity_constitutes_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('fcf62519-a854-46f2-9a67-c6eeba42ec76', bloodline_continuity_constitutes_legitimacy, conventional).
narrative_ontology:cs_reference_frame('fcf62519-a854-46f2-9a67-c6eeba42ec76', divine_right_bloodline_continuity).
narrative_ontology:cs_drift_state('fcf62519-a854-46f2-9a67-c6eeba42ec76', post_enlightenment_popular_sovereignty_era, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('fcf62519-a854-46f2-9a67-c6eeba42ec76', '').
narrative_ontology:cs_kernel_id(sovereign_legitimacy__monarchical_reading, sovereign_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sovereign_legitimacy__monarchical_reading, hereditary_ruling_dynasty).
narrative_ontology:constraint_beneficiary(sovereign_legitimacy__monarchical_reading, landed_aristocracy).
narrative_ontology:constraint_beneficiary(sovereign_legitimacy__monarchical_reading, established_church_hierarchy).
narrative_ontology:constraint_victim(sovereign_legitimacy__monarchical_reading, commoner_subjects).
narrative_ontology:constraint_victim(sovereign_legitimacy__monarchical_reading, excluded_collateral_bloodlines).
narrative_ontology:constraint_victim(sovereign_legitimacy__monarchical_reading, urban_merchant_class).
narrative_ontology:constraint_vindicates(sovereign_legitimacy__monarchical_reading, doctrine_of_divine_right).
narrative_ontology:constraint_vindicates(sovereign_legitimacy__monarchical_reading, principle_of_bloodline_continuity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds the throne by claim of unbroken bloodline, administers succession law, courts, and the apparatus of coronation ritual. Collects tribute, land rents, and deference as the direct yield of the arrangement. Can revise succession rules, adjust ceremonial emphasis, or reinterpret divine sanction as circumstances demand — the one seat with genuine discretion over the constraint's terms.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__monarchical_reading, hereditary_ruling_dynasty, agenda_setter,
    institutional, civilizational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(sovereign_legitimacy__monarchical_reading, hereditary_ruling_dynasty, beneficiary).

% Holds delegated authority, land, and title contingent on loyalty to the crown and the bloodline principle that legitimizes their own inherited stations. Benefits from the same logic that legitimizes the sovereign; would lose standing if authority were re-grounded in consent or merit.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__monarchical_reading, landed_aristocracy, beneficiary,
    powerful, generational, constrained, regional).

% Supplies the doctrinal machinery of divine sanction — coronation rites, oaths, theological justification for bloodline succession. Receives patronage, land grants, and protected status in exchange for continuously re-certifying the sovereign's legitimacy. Its authority and the crown's are mutually load-bearing.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__monarchical_reading, established_church_hierarchy, beneficiary,
    institutional, civilizational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(sovereign_legitimacy__monarchical_reading, established_church_hierarchy, agenda_setter).

% Owe obedience, taxation, and labor obligations to a hierarchy they had no part in selecting and cannot alter through any sanctioned channel. Exit means exile, rebellion, or quiet noncompliance — all carrying severe risk. Bear the cost of maintaining a legitimacy claim from which they draw no participatory return.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__monarchical_reading, commoner_subjects, payer,
    powerless, biographical, trapped, national).

% Possess plausible hereditary claims of their own but are structurally shut out by primogeniture rules, birth order, or legitimacy disputes over marriage and parentage. Their exclusion is precisely what succession law exists to adjudicate and foreclose; they are the perennial source of contested-succession crises.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__monarchical_reading, excluded_collateral_bloodlines, payer,
    moderate, generational, constrained, national).

% Accumulates capital and practical influence but is denied formal political standing because legitimacy is defined by blood, not wealth or competence. Pays taxes and tariffs set by an authority structure it cannot enter regardless of accomplishment, generating chronic friction between economic and political power.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__monarchical_reading, urban_merchant_class, payer,
    moderate, biographical, constrained, regional).

% Study the monarchical legitimacy claim comparatively against republican and hybrid readings, examining succession crises, coronation ritual, and doctrinal shifts in divine-right theory as evidence of the arrangement's actual versus claimed function.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__monarchical_reading, constitutional_theorists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(sovereign_legitimacy__monarchical_reading, hereditary_ruling_dynasty).
narrative_ontology:fixing_cost_class(sovereign_legitimacy__monarchical_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a determinate, low-negotiation-cost rule for resolving who holds supreme authority — birth order and bloodline settle the question without requiring recurrent contests, in principle preventing continuous factional warfare over the throne.
% TRANSFER_FUNCTION: Moves labor, taxation, deference, and political voice from the general population upward to the ruling dynasty, the aristocracy, and the church hierarchy that jointly administer and certify the succession claim.
% ABSENT_VOICES: Commoner subjects, excluded collateral bloodlines, and the rising merchant class have no sanctioned channel to contest the legitimacy premise itself; they may contest a particular succession outcome but not the bloodline-and-divine-sanction framework that produces all outcomes.
% DISAPPEARANCE_RATIONALE: If the doctrine vanished overnight, the entire apparatus of coronation, primogeniture law, aristocratic privilege, and church-state mutual certification would lose its organizing logic; land title, tax obligation, and political standing would all require re-grounding in some other principle (consent, merit, force) — a wholesale reorganization of the political order, not a cosmetic change.
% FOUNDING_PROBLEM: Pre-modern polities faced recurring, often violent contests over who should rule; a determinate hereditary rule backed by religious sanction was constructed to settle succession without requiring war or negotiation at every transition.
% FOUNDING_PROBLEM_CORROBORATION: The dynasty and church attest the problem remains live — that bloodline succession still prevents factional chaos. Independent historians and constitutional theorists outside the beneficiary set observe that hereditary succession itself is a leading historical cause of civil war and succession crisis (the excluded-collateral-bloodline problem), suggesting the arrangement as often produces the disorder it claims to prevent as it forecloses it.
narrative_ontology:disappearance_verdict(sovereign_legitimacy__monarchical_reading, world_rearranges).
narrative_ontology:founding_problem_status(sovereign_legitimacy__monarchical_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sovereign_legitimacy__monarchical_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(sovereign_legitimacy__monarchical_reading, 'none', 1).
narrative_ontology:epsilon_provenance(sovereign_legitimacy__monarchical_reading, 0.78, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sovereign_legitimacy__monarchical_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(sovereign_legitimacy__monarchical_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(sovereign_legitimacy__monarchical_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is high and rising (0.60 to 0.78 across the interval) because the coordination function — settling succession without recurrent war — is real but thin relative to the ongoing transfer of labor, taxation, and political voice it justifies. Suppression is very high (0.87) and rising because the arrangement's persistence depends on actively foreclosing rival legitimacy claims (republican, meritocratic, or rival-bloodline) rather than on voluntary participant endorsement. Theater ratio climbs to 0.42 as coronation ritual, court ceremony, and doctrinal reassertion increasingly substitute for the arrangement's original conflict-prevention function, especially visible whenever succession crises reveal that bloodline rules do not reliably prevent contest — they merely relocate it to disputes over legitimacy of birth and marriage.
 *
 * PERSPECTIVAL GAP:
 *   From the dynasty's and church's seats, the arrangement is genuine coordination: a settled, ritual-validated answer to a genuinely hard problem (who rules). From the commoner-subject and excluded-bloodline seats, the same structure computes as extraction backed by suppression — obedience and taxation flow upward through a hierarchy they cannot enter or contest. The engine should compute these as different seat-level types from the same structural data; the divergence is the object of study, not an error to reconcile.
 *
 * DIRECTIONALITY LOGIC:
 *   The dynasty and church sit at the beneficiary end of directionality: they set terms, are certified by their own doctrinal apparatus, and hold arbitrage-grade exit (they can reinterpret or adjust doctrine as circumstances require). The aristocracy is a derivative beneficiary — dependent on the same logic but without the dynasty's discretion. Commoner subjects sit at the full-target end: trapped exit, no participatory channel, bearing the transfer without derived benefit. Excluded collateral bloodlines and the merchant class are targets by exclusion rather than by direct extraction — the arrangement's rules are precisely what forecloses their claims to standing.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (violent, unresolved succession contests) is genuinely serious and the coordination logic is not invented — but its status here is contested rather than settled: hereditary succession is documented as itself a major historical driver of civil war (contested successions, disputed legitimacy of birth or marriage) as often as it prevents disorder. This is exactly the divergence the classification exists to surface — a claimed rope-like coordination function (settling succession peacefully) that the metrics and historical record show operating substantially as tangled_rope: real coordination value bundled with asymmetric, actively-enforced extraction from those excluded from authority.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    monarchical_reading_vs_siblings,
    'Is the monarchical reading of sovereign legitimacy a distinct, coherent commitment framework, or does it survive only as a degraded residue once republican and hybrid readings have displaced it in most functioning states?',
    'Comparative constitutional survey: count polities where pure hereditary-divine-right legitimacy remains the operative legal doctrine (as opposed to symbolic/ceremonial residue within a hybrid framework) versus polities where it has been formally superseded.',
    'If the monarchical reading survives in only ceremonial or symbolic form nearly everywhere, this constraint itself may be better classified as piton (vestigial, theater-heavy) rather than tangled_rope in most contemporary instances — though as authored here, for states where it remains the operative legitimacy doctrine, tangled_rope stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(monarchical_reading_vs_siblings, conceptual, 'Whether the monarchical reading is a live commitment framework or a residual one in the contemporary landscape.').

omega_variable(
    divine_sanction_naturalness,
    'Is the claim of divine sanction underlying bloodline succession a genuine metaphysical commitment held in good faith by its adherents, or a constructed legitimating narrative maintained because it benefits the dynasty and church?',
    'Historical and theological analysis of doctrinal shifts in divine-right theory correlated with political convenience — e.g., reinterpretations of succession doctrine that conveniently favor whichever claimant currently holds power.',
    'If divine sanction doctrine shifts opportunistically with political circumstance, this strengthens the case that the coordination story (peaceful succession) is cover for extraction rather than the arrangement''s true function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(divine_sanction_naturalness, conceptual, 'Whether divine sanction is sincere doctrine or constructed legitimation.').

omega_variable(
    succession_crisis_frequency,
    'Does bloodline succession empirically reduce violent contests for power relative to alternative legitimacy mechanisms, or does it merely relocate the contest to disputes over legitimacy of birth, marriage, and primogeniture?',
    'Historical frequency analysis of succession-related civil conflict under hereditary-monarchical systems versus republican and hybrid systems across comparable time periods.',
    'If hereditary succession does not reduce violent contest frequency relative to alternatives, the coordination function claimed as the arrangement''s justification is substantially undermined, strengthening a snare-leaning reading over tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(succession_crisis_frequency, empirical, 'Whether hereditary succession actually delivers the stability it claims as its coordination function.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sovereign_legitimacy__monarchical_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sove_tr_t0, sovereign_legitimacy__monarchical_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(sove_tr_t8, sovereign_legitimacy__monarchical_reading, theater_ratio, 8, 0.26).
narrative_ontology:measurement(sove_tr_t16, sovereign_legitimacy__monarchical_reading, theater_ratio, 16, 0.32).
narrative_ontology:measurement(sove_tr_t24, sovereign_legitimacy__monarchical_reading, theater_ratio, 24, 0.36).
narrative_ontology:measurement(sove_tr_t32, sovereign_legitimacy__monarchical_reading, theater_ratio, 32, 0.39).
narrative_ontology:measurement(sove_tr_t40, sovereign_legitimacy__monarchical_reading, theater_ratio, 40, 0.42).

% Extraction over time
narrative_ontology:measurement(sove_be_t0, sovereign_legitimacy__monarchical_reading, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(sove_be_t8, sovereign_legitimacy__monarchical_reading, base_extractiveness, 8, 0.65).
narrative_ontology:measurement(sove_be_t16, sovereign_legitimacy__monarchical_reading, base_extractiveness, 16, 0.7).
narrative_ontology:measurement(sove_be_t24, sovereign_legitimacy__monarchical_reading, base_extractiveness, 24, 0.74).
narrative_ontology:measurement(sove_be_t32, sovereign_legitimacy__monarchical_reading, base_extractiveness, 32, 0.77).
narrative_ontology:measurement(sove_be_t40, sovereign_legitimacy__monarchical_reading, base_extractiveness, 40, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(sove_su_t0, sovereign_legitimacy__monarchical_reading, suppression_requirement, 0, 0.68).
narrative_ontology:measurement(sove_su_t8, sovereign_legitimacy__monarchical_reading, suppression_requirement, 8, 0.74).
narrative_ontology:measurement(sove_su_t16, sovereign_legitimacy__monarchical_reading, suppression_requirement, 16, 0.79).
narrative_ontology:measurement(sove_su_t24, sovereign_legitimacy__monarchical_reading, suppression_requirement, 24, 0.83).
narrative_ontology:measurement(sove_su_t32, sovereign_legitimacy__monarchical_reading, suppression_requirement, 32, 0.85).
narrative_ontology:measurement(sove_su_t40, sovereign_legitimacy__monarchical_reading, suppression_requirement, 40, 0.87).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sovereign_legitimacy__monarchical_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(sovereign_legitimacy__monarchical_reading, republican_reading).
narrative_ontology:affects_constraint(sovereign_legitimacy__monarchical_reading, constitutional_hybrid_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three members of the sovereign_legitimacy kernel family. republican_reading holds the mirror-image premise (authority ascends from consent) and stands in a forecloses relation to this reading — no single framework can hold both a downward-flowing divine-right claim and an upward-flowing popular-sovereignty claim as simultaneously foundational. constitutional_hybrid_reading partially absorbs this reading's ceremonial/traditional component while delegating political authority elsewhere, standing in an influences relation: this reading's persistence and doctrinal maintenance shapes the resource availability and legitimacy conditions the hybrid reading operates under (e.g., surviving constitutional monarchies). Each story authors its own ε, beneficiary/victim structure, and classification independently, per the ε-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
