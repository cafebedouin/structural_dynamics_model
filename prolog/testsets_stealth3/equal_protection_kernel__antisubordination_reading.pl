% ============================================================================
% CONSTRAINT STORY: equal_protection_kernel__antisubordination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_equal_protection_kernel__antisubordination_reading, []).

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
 *   constraint_id: equal_protection_kernel__antisubordination_reading
 *   human_readable: Equal Protection Clause — Antisubordination Reading
 *   domain: constitutional law / civil rights / education policy
 *
 * SUMMARY:
 *   State action that entrenches caste-like hierarchy is constitutionally
 *   forbidden; state action that dismantles such hierarchy is
 *   constitutionally permitted and shielded. This is the antisubordination
 *   instantiation of the equal protection guarantee: the constitutional
 *   injury is the subordination of historically oppressed castes, not the use
 *   of classifications as such. The constraint is enforced by the federal
 *   judiciary, which strikes down hierarchy-entrenching state programs and
 *   shields remedial ones. Its protection runs to subordinated racial castes
 *   and historically oppressed religious minorities; its costs fall on
 *   historically dominant groups — which this reading does not recognize as
 *   constitutional injured parties — and on state actors whose preferred
 *   classifications are invalidated. Built against the Reconstruction-era
 *   Black Codes, the constraint has passed through construction (1954–1971),
 *   peak remedial enforcement (1970s–1980s), and a sustained decay of
 *   enforcement machinery since, leaving a core prohibition that still
 *   functions and a remedial limb maintained increasingly by rhetoric. The
 *   claim and the metrics are independent authored facts: the claimed type
 *   states the structure I believe true; the metrics describe the
 *   constraint's current, partially hollowed operation.
 *
 * KEY AGENTS:
 *   - subordinated_racial_castes: Primary beneficiary (moderate/identity_locked) — protected against state-entrenched hierarchy; collects remedial license; cannot exit the identity the protection attaches to
 *   - historically_dominant_groups: Primary cost-bearer (powerful/constrained) — bears remedial burdens without constitutional recourse under this reading
 *   - federal_judiciary: Agenda-setter/enforcer (institutional/constrained) — administers the constraint, collects institutional authority from enforcement, absorbs legitimacy costs
 *   - state_governments: Dual-positioned governed actor (institutional/constrained) — forbidden hierarchy-entrenching action, licensed dismantling action
 *   - civil_rights_advocacy_organizations: Secondary beneficiary (organized/constrained) — litigates under the framework; standing, funding, and mission identity bound to its vitality
 *   - asian_american_communities: Excluded claimant (moderate/identity_locked) — subordination history outside the binary beneficiary/payer structure
 *   - constitutional_law_scholars: Analytical observer (analytical/analytical) — sees the full structure including the enforcement-decay trajectory
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equal_protection_kernel__antisubordination_reading, 0.18).
domain_priors:suppression_score(equal_protection_kernel__antisubordination_reading, 0.25).
domain_priors:theater_ratio(equal_protection_kernel__antisubordination_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equal_protection_kernel__antisubordination_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(equal_protection_kernel__antisubordination_reading, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(equal_protection_kernel__antisubordination_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equal_protection_kernel__antisubordination_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(equal_protection_kernel__antisubordination_reading, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equal_protection_kernel__antisubordination_reading, tangled_rope).
narrative_ontology:human_readable(equal_protection_kernel__antisubordination_reading, "Equal Protection Clause — Antisubordination Reading").
narrative_ontology:topic_domain(equal_protection_kernel__antisubordination_reading, "constitutional law / civil rights / education policy").

domain_priors:requires_active_enforcement(equal_protection_kernel__antisubordination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equal_protection_kernel__antisubordination_reading, '17a835e5-af3b-4dc0-8c48-8bb354c47e3f').
narrative_ontology:cs_kernel_codification('17a835e5-af3b-4dc0-8c48-8bb354c47e3f', fixed_text).
narrative_ontology:cs_authority_grounding('17a835e5-af3b-4dc0-8c48-8bb354c47e3f', lineage).
narrative_ontology:cs_interpretation_layer_present('17a835e5-af3b-4dc0-8c48-8bb354c47e3f').
narrative_ontology:cs_reading_relation('17a835e5-af3b-4dc0-8c48-8bb354c47e3f', equal_protection_kernel__remedial_reading, influences).
narrative_ontology:cs_reading_relation('17a835e5-af3b-4dc0-8c48-8bb354c47e3f', equal_protection_kernel__colorblind_reading, forecloses).
narrative_ontology:cs_axiom('17a835e5-af3b-4dc0-8c48-8bb354c47e3f', foundational, caste_subordination_constitutional_injury).
narrative_ontology:cs_axiom_status(caste_subordination_constitutional_injury, holdable).
narrative_ontology:cs_axiom_grounding('17a835e5-af3b-4dc0-8c48-8bb354c47e3f', caste_subordination_constitutional_injury, deontological).
narrative_ontology:cs_axiom('17a835e5-af3b-4dc0-8c48-8bb354c47e3f', secondary, hierarchy_dismantling_state_action_permitted).
narrative_ontology:cs_axiom_status(hierarchy_dismantling_state_action_permitted, holdable).
narrative_ontology:cs_axiom_grounding('17a835e5-af3b-4dc0-8c48-8bb354c47e3f', hierarchy_dismantling_state_action_permitted, instrumental).
narrative_ontology:cs_reference_frame('17a835e5-af3b-4dc0-8c48-8bb354c47e3f', reconstruction_anticaste_baseline).
narrative_ontology:cs_drift_state('17a835e5-af3b-4dc0-8c48-8bb354c47e3f', contemporary_post_sffa_doctrine, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('17a835e5-af3b-4dc0-8c48-8bb354c47e3f', '').
narrative_ontology:cs_kernel_id(equal_protection_kernel__antisubordination_reading, equal_protection_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equal_protection_kernel__antisubordination_reading, subordinated_racial_castes).
narrative_ontology:constraint_beneficiary(equal_protection_kernel__antisubordination_reading, historically_oppressed_religious_minorities).
narrative_ontology:constraint_victim(equal_protection_kernel__antisubordination_reading, historically_dominant_groups).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(equal_protection_kernel__antisubordination_reading, state_governments).
narrative_ontology:constraint_beneficiary(equal_protection_kernel__antisubordination_reading, civil_rights_advocacy_organizations).
narrative_ontology:constraint_victim(equal_protection_kernel__antisubordination_reading, state_governments).
narrative_ontology:constraint_vindicates(equal_protection_kernel__antisubordination_reading, antiestablishment_principle).
narrative_ontology:constraint_vindicates(equal_protection_kernel__antisubordination_reading, structural_disparity_evidence_relevance).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Adjudicates which uses of state power entrench caste-like hierarchy and which aim at dismantling it; invalidates the former and shields the latter. Its docket, jurisdiction, and institutional standing grow with each enforcement episode, and it absorbs legitimacy costs when its rulings narrow against political majorities. It is bound by precedent and jurisdiction, though it can reverse its own doctrine at high cost.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__antisubordination_reading, federal_judiciary, agenda_setter,
    institutional, generational, constrained, national).

% Live under the guarantee that state action may not re-entrench the hierarchy that subordinated them, and hold the license to seek remedial measures. The protection is only as strong as enforcement they do not control; their weak political position in hostile jurisdictions is what makes the guarantee necessary. They cannot exit the identity the guarantee attaches to.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__antisubordination_reading, subordinated_racial_castes, beneficiary,
    moderate, generational, identity_locked, national).

% Hold protection against state action that would establish or entrench religious hierarchy — the clause's original application beyond race. Conversion or concealment is a possible but costly exit; their protection likewise depends on enforcement they do not control.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__antisubordination_reading, historically_oppressed_religious_minorities, beneficiary,
    moderate, generational, constrained, national).

% Bear the costs of remedial measures — foreclosed opportunities, displaced preferences, reassigned burdens — and, under this reading, hold no claim under the clause against those burdens. They retain ordinary political participation, but the guarantee exists precisely to override that participation where it turns to hierarchy. Their recourse is to contest the reading itself, not to claim its protection.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__antisubordination_reading, historically_dominant_groups, payer,
    powerful, biographical, constrained, national).

% Operate under a two-sided rule: action that entrenches hierarchy is struck down, action that dismantles it is shielded. Individual states are simultaneously restricted (their preferred classifications invalidated) and empowered (their remedial programs given constitutional cover). They have no exit from the supremacy of the constitutional rule.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__antisubordination_reading, state_governments, payer,
    institutional, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(equal_protection_kernel__antisubordination_reading, state_governments, beneficiary).

% Litigate, organize, and fundraise under the framework this reading supplies; their standing, funding base, and mission identity are bound to its vitality. They could reorganize around adjacent missions, but the cost of mission drift is high.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__antisubordination_reading, civil_rights_advocacy_organizations, beneficiary,
    organized, generational, constrained, national).

% Carry a distinct subordination record — exclusion acts, internment, alien land laws — that maps onto neither the clean protected seat nor the clean cost-bearing seat. In some remedial contexts they are treated as cost-bearers, in others as unprotected; they would claim full membership in the subordination framework, but the binary application of the reading leaves their claims unadjudicated.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__antisubordination_reading, asian_american_communities, excluded,
    moderate, generational, identity_locked, national).

% Study the clause's structure across jurisdictions and eras; they see the full trajectory — construction, peak enforcement, decay — and the gap between what the text's Reconstruction-era authors committed to and what current doctrine sustains.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__antisubordination_reading, constitutional_law_scholars, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(equal_protection_kernel__antisubordination_reading, diffuse).
narrative_ontology:fixing_cost_class(equal_protection_kernel__antisubordination_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a counter-majoritarian guarantee that no political coalition could reliably supply: a stable constitutional rule that state power may not be used to entrench caste hierarchy, plus a shared standard distinguishing hierarchy-entrenching from hierarchy-dismantling uses of racial and religious classification.
% TRANSFER_FUNCTION: Moves constitutional protection and remedial license toward historically subordinated castes; moves the costs of remediation (foreclosed opportunities, invalidated programs) onto historically dominant groups and onto state actors pursuing hierarchical ends; moves adjudication authority over race and state power to the federal judiciary.
% ABSENT_VOICES: Communities with subordination histories outside the recognized binary — notably Asian American communities whose exclusion history maps onto neither the clean beneficiary nor the clean payer seat — would object to the framework's binary application. Dominant-group individuals seeking individualized assessment have no procedural seat under this reading's foreclosure of their claims; they are present in politics but absent from the constitutional conversation this reading defines.
% DISAPPEARANCE_RATIONALE: If the constraint vanished overnight, state governments in hostile jurisdictions could re-entrench caste through overt and facially neutral policy without constitutional barrier; remedial programs would lose their license; the subordinated castes' protection would fall back to ordinary majoritarian politics, which the founding record shows fails exactly where subordination is popular.
% FOUNDING_PROBLEM: Post-Civil War state governments enacted Black Codes re-entrenching a racial caste system through law; ordinary politics could not check this because the subordination was majoritarian in the affected jurisdictions. The clause was built to give the abolition of caste permanent, judicially enforceable constitutional force against state action.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated outside the beneficiary set by researchers with no stake in the advocacy framework: federal statistical agencies and academic economists document persistent caste-patterned disparities in wealth, incarceration, housing, and health; historical scholarship on the 39th Congress corroborates the founding problem's original content. No corroborating source outside the dispute attests that the problem is resolved.
narrative_ontology:disappearance_verdict(equal_protection_kernel__antisubordination_reading, world_rearranges).
narrative_ontology:founding_problem_status(equal_protection_kernel__antisubordination_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equal_protection_kernel__antisubordination_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(equal_protection_kernel__antisubordination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(equal_protection_kernel__antisubordination_reading, 0.18, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equal_protection_kernel__antisubordination_reading_tests).
:- end_tests(equal_protection_kernel__antisubordination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored low (0.18) because the reading counts the constraint's costs — remedial burdens on dominant groups, invalidated hierarchical programs — as correction rather than taking; the residual extraction is real but small, and it has shrunk as the remedial permissions were narrowed. Suppression is authored at 0.25 as a raw structural property (only extractiveness is scaled by directionality and scope in the engine's computation): the constraint's coercive enforcement machinery has been largely dismantled, and what remains is a thinner judicial hold. Theater is 0.45 because a substantial share of the activity around the constraint is now commemorative — the norm is recited, taught, and celebrated while its remedial machinery is hollow — though the core prohibition on formal caste law still functions, keeping theater below half. Accessibility collapse is 0.5: a rival framework for reading the same text persists as a live alternative, so understanding this constraint does not collapse the alternatives. Resistance is 0.65: the constraint has met sustained political and doctrinal resistance throughout its life, and that resistance has been winning for two generations. The suppression_requirement series is authored because the story specifically tracks enforcement-capacity change — construction, peak, decay — not a static enforcement picture. All three metrics run on one shared time grid (1954–2024, decade points) with a value authored at every point; the arc is rise-peak-decay, not cyclical.
 *
 * PERSPECTIVAL GAP:
 *   The payer seat and the beneficiary seat compute this constraint oppositely, and the gap is the reading's own design. From the subordinated castes' seat the arrangement is a protective guarantee they cannot exit and do not control — near-pure protection, low effective burden. From the dominant groups' seat the same arrangement operates as enforced burden: they pay remedial costs and are told, by the reading itself, that they have no claim. The federal judiciary's seat is dual — it administers the constraint and collects institutional authority from administering it, while absorbing legitimacy costs when enforcement narrows. State governments at the same nominal institutional level split by position: those whose programs are struck experience the rule as prohibition; those running remedial programs experience it as cover. An analytical seat sees both the genuine anti-caste function and the asymmetric burden structure, plus the decay trajectory. The engine computes this divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary declarations (subordinated racial castes, historically oppressed religious minorities) drive those seats toward the beneficiary end; identity-locked exit pins the racial-caste seat nearest full beneficiary, since the protection attaches to an identity its holders cannot shed. The victim declaration (historically dominant groups) drives that seat toward the target end: exit is constrained because the political route is exactly what the constraint overrides and the claim route is closed under this reading — the gap between bearing the constraint's costs and being recognized as injured by it is the reading's defining asymmetry, and it keeps their effective burden near the full-target end despite their majoritarian political power. The federal judiciary sits mid-range: neither declared beneficiary nor declared victim, it collects administrative authority and bears legitimacy costs. State governments carry a genuinely dual position — restricted on hierarchical action, shielded on remedial action — which the derivation handles through their dual role rather than an override. No directionality overrides are authored: the beneficiary/victim declarations plus exit atoms produce the correct structure for every seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — majoritarian re-entrenchment of caste that ordinary politics cannot check — is live, so the constraint is not a mandate outliving its function, and the classification must not read the enforcement decay as mandatrophy. What has decayed is the remedial limb's enforcement, not the founding problem. Reading the low current extraction as 'harmless coordination now' would miss the payer seat's live burden structure; reading the payer burden as pure extraction would miss the anti-caste function that still operates at the core and the beneficiary class that still depends on it. The combined coordination/extraction reading, with the decay tracked temporally, keeps both errors out. The theater rise tracks the hollowing of enforcement, not the death of the founding problem — a distinction the founding_problem_status 'live' declaration carries.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'This constraint is one reading of the equal_protection_kernel; what would the sibling readings change structurally, and where exactly is the disagreement located?',
    'Doctrinal evolution: which reading a Supreme Court majority adopts determines the operative victim set (anyone racially classified vs. those suffering subordination vs. those excluded by documented historical discrimination) and the beneficiary set (none vs. subordinated castes vs. remedial-target groups).',
    'Adoption of the colorblind sibling eliminates this constraint''s beneficiary structure entirely — every remedial permission falls and the victim set expands to anyone classified. Adoption of the remedial sibling narrows the dismantling license to documented-exclusion cases. The disagreement is located at the injury question: subordination versus classification versus exclusion.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Committer structure: one of three readings of the equal protection kernel; siblings would change victim and beneficiary sets.').

omega_variable(
    caste_analogy_scope,
    'Does ''caste-like subordination'' extend beyond the Black-white binary to groups whose subordination took other forms — Asian American exclusion, Native dispossession, religious hierarchy — and who adjudicates membership in the protected set?',
    'Comparative constitutional analysis (India''s caste doctrine, South Africa''s equality jurisprudence) and doctrinal development of which groups'' histories count as caste-like.',
    'A broader protected set multiplies the remedial license and complicates the payer structure; a narrower binary preserves the current structure but leaves the excluded claimants outside the framework entirely.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(caste_analogy_scope, conceptual, 'Scope of the protected class under the caste analogy.').

omega_variable(
    enforcement_decay_trajectory,
    'Is the decay of the constraint''s enforcement machinery (suppression_requirement falling from its 1964 peak to 0.25) a permanent doctrinal shift or one phase in a longer cycle of construction and retrenchment?',
    'Observe whether a future doctrinal majority revives the remedial permissions and disparate-impact analysis, or whether the hollowing consolidates.',
    'Permanent decay drives the constraint toward theatrical maintenance of a hollowed remedial limb while the core prohibition persists; cyclical reversal restores the combined coordination/extraction structure at higher extractiveness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_decay_trajectory, empirical, 'Whether enforcement decay is terminal or cyclical.').

omega_variable(
    dominant_group_burden_status,
    'Are the burdens this reading imposes on members of historically dominant groups — foreclosed opportunities under remedial measures, with no claim under the clause — constitutional injuries, or the legitimate dismantling of unearned advantage?',
    'Not resolvable by data alone: this reading answers it by definition (not injuries); the rival readings answer the opposite. Resolution comes only from a shift in which reading governs, or from a principled account of when remedial burden becomes injury even within the antisubordination frame.',
    'If the burdens are injuries, the constraint is substantially more extractive than authored and the payer seat''s claim is valid; if they are not, the low epsilon stands and the asymmetry is the reading''s point rather than its defect.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(dominant_group_burden_status, preference, 'The normative core of the reading contest: whether dominant-group remedial burdens count as injury.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equal_protection_kernel__antisubordination_reading, 1954, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(equa_tr_t1954, equal_protection_kernel__antisubordination_reading, theater_ratio, 1954, 0.35).
narrative_ontology:measurement_basis(equa_tr_t1954, observed).
narrative_ontology:measurement(equa_tr_t1964, equal_protection_kernel__antisubordination_reading, theater_ratio, 1964, 0.2).
narrative_ontology:measurement_basis(equa_tr_t1964, observed).
narrative_ontology:measurement(equa_tr_t1974, equal_protection_kernel__antisubordination_reading, theater_ratio, 1974, 0.15).
narrative_ontology:measurement_basis(equa_tr_t1974, observed).
narrative_ontology:measurement(equa_tr_t1984, equal_protection_kernel__antisubordination_reading, theater_ratio, 1984, 0.18).
narrative_ontology:measurement_basis(equa_tr_t1984, observed).
narrative_ontology:measurement(equa_tr_t1994, equal_protection_kernel__antisubordination_reading, theater_ratio, 1994, 0.25).
narrative_ontology:measurement_basis(equa_tr_t1994, observed).
narrative_ontology:measurement(equa_tr_t2004, equal_protection_kernel__antisubordination_reading, theater_ratio, 2004, 0.3).
narrative_ontology:measurement_basis(equa_tr_t2004, observed).
narrative_ontology:measurement(equa_tr_t2014, equal_protection_kernel__antisubordination_reading, theater_ratio, 2014, 0.38).
narrative_ontology:measurement_basis(equa_tr_t2014, observed).
narrative_ontology:measurement(equa_tr_t2024, equal_protection_kernel__antisubordination_reading, theater_ratio, 2024, 0.45).
narrative_ontology:measurement_basis(equa_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(equa_be_t1954, equal_protection_kernel__antisubordination_reading, base_extractiveness, 1954, 0.15).
narrative_ontology:measurement_basis(equa_be_t1954, observed).
narrative_ontology:measurement(equa_be_t1964, equal_protection_kernel__antisubordination_reading, base_extractiveness, 1964, 0.2).
narrative_ontology:measurement_basis(equa_be_t1964, observed).
narrative_ontology:measurement(equa_be_t1974, equal_protection_kernel__antisubordination_reading, base_extractiveness, 1974, 0.3).
narrative_ontology:measurement_basis(equa_be_t1974, observed).
narrative_ontology:measurement(equa_be_t1984, equal_protection_kernel__antisubordination_reading, base_extractiveness, 1984, 0.38).
narrative_ontology:measurement_basis(equa_be_t1984, observed).
narrative_ontology:measurement(equa_be_t1994, equal_protection_kernel__antisubordination_reading, base_extractiveness, 1994, 0.32).
narrative_ontology:measurement_basis(equa_be_t1994, observed).
narrative_ontology:measurement(equa_be_t2004, equal_protection_kernel__antisubordination_reading, base_extractiveness, 2004, 0.28).
narrative_ontology:measurement_basis(equa_be_t2004, observed).
narrative_ontology:measurement(equa_be_t2014, equal_protection_kernel__antisubordination_reading, base_extractiveness, 2014, 0.22).
narrative_ontology:measurement_basis(equa_be_t2014, observed).
narrative_ontology:measurement(equa_be_t2024, equal_protection_kernel__antisubordination_reading, base_extractiveness, 2024, 0.18).
narrative_ontology:measurement_basis(equa_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(equa_su_t1954, equal_protection_kernel__antisubordination_reading, suppression_requirement, 1954, 0.3).
narrative_ontology:measurement_basis(equa_su_t1954, observed).
narrative_ontology:measurement(equa_su_t1964, equal_protection_kernel__antisubordination_reading, suppression_requirement, 1964, 0.6).
narrative_ontology:measurement_basis(equa_su_t1964, observed).
narrative_ontology:measurement(equa_su_t1974, equal_protection_kernel__antisubordination_reading, suppression_requirement, 1974, 0.55).
narrative_ontology:measurement_basis(equa_su_t1974, observed).
narrative_ontology:measurement(equa_su_t1984, equal_protection_kernel__antisubordination_reading, suppression_requirement, 1984, 0.45).
narrative_ontology:measurement_basis(equa_su_t1984, observed).
narrative_ontology:measurement(equa_su_t1994, equal_protection_kernel__antisubordination_reading, suppression_requirement, 1994, 0.35).
narrative_ontology:measurement_basis(equa_su_t1994, observed).
narrative_ontology:measurement(equa_su_t2004, equal_protection_kernel__antisubordination_reading, suppression_requirement, 2004, 0.3).
narrative_ontology:measurement_basis(equa_su_t2004, observed).
narrative_ontology:measurement(equa_su_t2014, equal_protection_kernel__antisubordination_reading, suppression_requirement, 2014, 0.27).
narrative_ontology:measurement_basis(equa_su_t2014, observed).
narrative_ontology:measurement(equa_su_t2024, equal_protection_kernel__antisubordination_reading, suppression_requirement, 2024, 0.25).
narrative_ontology:measurement_basis(equa_su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equal_protection_kernel__antisubordination_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(equal_protection_kernel__antisubordination_reading, equal_protection_kernel__remedial_reading).
narrative_ontology:affects_constraint(equal_protection_kernel__antisubordination_reading, equal_protection_kernel__colorblind_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'equal protection' covers three structurally distinct constraints — one per reading of the shared text. They differ in epsilon, in victim set (anyone classified / the subordinated / the documented-excluded), and in beneficiary set (none / subordinated castes / remedial-target groups). This file is the antisubordination member; the family is linked via affects_constraints so drift and contamination propagate across the readings. Each sibling file should carry a matching note.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
