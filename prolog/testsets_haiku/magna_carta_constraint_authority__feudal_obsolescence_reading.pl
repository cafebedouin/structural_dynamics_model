% ============================================================================
% CONSTRAINT STORY: magna_carta_constraint_authority__feudal_obsolescence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_magna_carta_constraint_authority__feudal_obsolescence_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: magna_carta_constraint_authority__feudal_obsolescence_reading
 *   human_readable: Magna Carta as Obsolete Feudal Document (Feudal Obsolescence Reading)
 *   domain: constitutional/legal/political
 *
 * SUMMARY:
 *   This constraint story instantiates the feudal obsolescence reading of the
 *   contested Magna Carta kernel. Under this reading, Magna Carta is
 *   understood as a historically-specific baronial compact addressing 13th
 *   century feudal grievances (arbitrary royal taxation, arbitrary justice,
 *   feudal custom violations) that has no binding authority over modern
 *   sovereignty structures. The reading permits executive authority to
 *   maximize discretion by declaring the charter's restraint language
 *   historically contingent and inapplicable. The constraint is claimed as a
 *   piton—theatrically maintained constitutional symbol whose operative
 *   restraint function has atrophied—because the reading invokes Magna
 *   Carta's historical importance while stripping it of binding legal effect.
 *   This reading coexists with living constitutionalism (treating the charter
 *   as evolving precedent) and parliamentary sovereignty (treating statutory
 *   law as the sole authority), but forecloses any framing that treats
 *   medieval feudal law as directly applicable to modern sovereigns. The
 *   constraint's measured extractiveness (0.62) reflects the gap between the
 *   charter's restraint narrative and its actual non-enforcement; suppression
 *   (0.58) reflects the institutional effort required to maintain the
 *   'historically obsolete' framing against judicial and popular
 *   constitutionalist challenge.
 *
 * KEY AGENTS:
 *   - Executive authority (modern state): maintains the feudal obsolescence reading and maximizes discretion unconstrained by charter restraints.
 *   - Popular constitutionalism advocates: assert that Magna Carta's restraint language binds modern sovereigns; excluded from institutional decision-making (identity-locked exit).
 *   - Judicial restraint advocates: argue courts should read Magna Carta as binding precedent; constrained by doctrines treating the charter as historical artifact.
 *   - Academic historians (contextualist school): benefit professionally from the feudal obsolescence reading; validate historical-artifact framing.
 *   - Constitutional theorists (living tradition): excluded from shaping the reading; would argue the charter persists through interpretive evolution.
 *   - Parliament: holds statutory authority over Magna Carta law but currently acquiesces to the feudal obsolescence reading.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magna_carta_constraint_authority__feudal_obsolescence_reading, 0.62).
domain_priors:suppression_score(magna_carta_constraint_authority__feudal_obsolescence_reading, 0.58).
domain_priors:theater_ratio(magna_carta_constraint_authority__feudal_obsolescence_reading, 0.71).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magna_carta_constraint_authority__feudal_obsolescence_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(magna_carta_constraint_authority__feudal_obsolescence_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(magna_carta_constraint_authority__feudal_obsolescence_reading, theater_ratio, 0.71).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(magna_carta_constraint_authority__feudal_obsolescence_reading, accessibility_collapse, 0.41).
narrative_ontology:constraint_metric(magna_carta_constraint_authority__feudal_obsolescence_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_constraint_authority__feudal_obsolescence_reading, piton).
narrative_ontology:human_readable(magna_carta_constraint_authority__feudal_obsolescence_reading, "Magna Carta as Obsolete Feudal Document (Feudal Obsolescence Reading)").
narrative_ontology:topic_domain(magna_carta_constraint_authority__feudal_obsolescence_reading, "constitutional/legal/political").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magna_carta_constraint_authority__feudal_obsolescence_reading, '449ad167-9c44-4b27-aa48-a6505b85086d').
narrative_ontology:cs_kernel_codification('449ad167-9c44-4b27-aa48-a6505b85086d', fixed_text).
narrative_ontology:cs_authority_grounding('449ad167-9c44-4b27-aa48-a6505b85086d', extraction).
narrative_ontology:cs_interpretation_layer_present('449ad167-9c44-4b27-aa48-a6505b85086d').
narrative_ontology:cs_reading_relation('449ad167-9c44-4b27-aa48-a6505b85086d', magna_carta_constraint_authority__living_constitutionalism_reading, forecloses).
narrative_ontology:cs_reading_relation('449ad167-9c44-4b27-aa48-a6505b85086d', magna_carta_constraint_authority__parliamentary_sovereignty_reading, coexists_with).
narrative_ontology:cs_axiom('449ad167-9c44-4b27-aa48-a6505b85086d', foundational, medieval_law_not_modern_authority).
narrative_ontology:cs_axiom_status(medieval_law_not_modern_authority, holdable).
narrative_ontology:cs_axiom_grounding('449ad167-9c44-4b27-aa48-a6505b85086d', medieval_law_not_modern_authority, conventional).
narrative_ontology:cs_axiom('449ad167-9c44-4b27-aa48-a6505b85086d', foundational, historical_context_determines_applicability).
narrative_ontology:cs_axiom_status(historical_context_determines_applicability, holdable).
narrative_ontology:cs_axiom_grounding('449ad167-9c44-4b27-aa48-a6505b85086d', historical_context_determines_applicability, empirically_contingent).
narrative_ontology:cs_reference_frame('449ad167-9c44-4b27-aa48-a6505b85086d', medieval_feudal_constraint_regime).
narrative_ontology:cs_drift_state('449ad167-9c44-4b27-aa48-a6505b85086d', contemporary_human_rights_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('449ad167-9c44-4b27-aa48-a6505b85086d', '').
narrative_ontology:cs_kernel_id(magna_carta_constraint_authority__feudal_obsolescence_reading, magna_carta_constraint_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magna_carta_constraint_authority__feudal_obsolescence_reading, executive_authority_modern_state).
narrative_ontology:constraint_victim(magna_carta_constraint_authority__feudal_obsolescence_reading, popular_constitutionalism_advocates).
narrative_ontology:constraint_victim(magna_carta_constraint_authority__feudal_obsolescence_reading, judicial_restraint_advocates).
narrative_ontology:constraint_victim(magna_carta_constraint_authority__feudal_obsolescence_reading, baronial_due_process_inheritors).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(magna_carta_constraint_authority__feudal_obsolescence_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(magna_carta_constraint_authority__feudal_obsolescence_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(magna_carta_constraint_authority__feudal_obsolescence_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(magna_carta_constraint_authority__feudal_obsolescence_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(magna_carta_constraint_authority__feudal_obsolescence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Theater ratio is high (0.71 at interval end) because the constraint persists through ceremonial invocation (Magna Carta remains symbolically central to constitutional mythology) while its operative restraint function is declared inert. The measurement series shows an upward arc from 1215 (low theater, real restraint function) through 1689 (English Civil War and Bill of Rights era, rising tension between historical charter and modern applications) to 1945–2026 (high theater, atrophied restraint). Extractiveness rises from 0.08 (feudal era, when the charter imposed real constraints on the sovereign) to 0.62 (modern era, when the sovereign can invoke historical context to avoid the restraint). Suppression requirement rises as the reading must actively foreclose rival interpretations (living constitutionalism, parliamentary incorporation) to maintain the feudal obsolescence framing. The measurement grid is shared across all three metrics; every metric is authored at every time point, showing the constraint's lifecycle from real feudal governance mechanism (low theater, low extraction) to modern piton (high theater, moderate extraction, persistent suppression requirement).
 *
 * PERSPECTIVAL GAP:
 *   From the executive authority seat, the feudal obsolescence reading is a reasonable historical interpretation that frees modern governance from medieval constraints. From the popular constitutionalism and judicial restraint seats, the same reading is a rhetorical device that strips the charter of binding force while maintaining its symbolic prestige—extracting the benefit of constitutional legitimacy while avoiding the cost of constraint. The engine should compute divergent types across these seats: the executive may experience this as a natural historical boundary (mountain-ish framing from their seat) while the excluded advocates experience it as an enforced constraint that prevents them from invoking the charter's restraint language (snare-ish or piton-ish from their seats). The authored metrics (high theater, moderate extraction, persistent suppression) align with the piton claim: the constraint is kept alive by institutional inertia and symbolic maintenance, not by genuine coordination benefit.
 *
 * DIRECTIONALITY LOGIC:
 *   Executive authority benefits from the feudal obsolescence reading: it permits discretion unconstrained by historical restraint language and collects the rhetorical benefit of constitutional legitimacy without paying the cost of operative constraint (d near 0.0, beneficiary end). Popular constitutionalists and judicial restraint advocates are the structural targets: they would invoke the charter's restraint language but the reading forecloses that invocation, forcing them to find alternative grounds for restraint or accept executive discretion (d near 1.0, target end). Academic historians benefit incidentally (their disciplinary interests align with historical contextualism) but are not the primary beneficiary—the executive is. The override to directionality comes through the suppression mechanism: the reading must actively suppress rival interpretations (living constitutionalism, statutory incorporation) to persist, which means suppression is structurally high relative to what a genuine historical boundary (mountain) would require.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint exhibits classic piton characteristics: (1) The founding problem (arbitrary feudal power) is dead (status: dead); the world would not rearrange if the feudal obsolescence reading disappeared, because modern statutory law and administrative procedure would persist. (2) No concentrated beneficiary maintains the constraint through genuine coordination—the executive benefits from the reading but does not actively defend it daily; courts and academics maintain the reading through inertia and disciplinary interest rather than active enforcement. (3) Theater is high and rising (0.71), indicating performative maintenance: Magna Carta is invoked ceremonially in constitutional discourse but its operative restraint is non-functional. (4) Suppression requirement is moderate (0.58) and stable—the effort required to maintain the 'historically obsolete' framing against challenge is real but not overwhelming, because the reading aligns with dominant institutional interests (executive discretion, academic contextualism). If the reading atrophied entirely and Magna Carta were reactivated as binding restraint, the institutional cost to the executive would be real but not catastrophic—statutory law and administrative procedure would provide alternative governance mechanisms. The mandate has not been superseded by a successor mechanism (making it a true piton); it persists because no single party benefits enough to fix it and no single party is hurt enough to rebel. The popular constitutionalists and judicial restraint advocates have constrained exit (identity-locked or professionally locked into restraint doctrine), so they suffer but lack the power to change the reading.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    feudal_vs_universal_restraint,
    'Is Magna Carta''s restraint language (due process, no arbitrary taxation, lawful governance) historically contingent to feudal law, or does it express universal principles of lawful sovereignty that bind all subsequent political orders?',
    'Comparative constitutional analysis: do modern constitutions reiterate Magna Carta''s restraint language as if it were universally binding rather than historically contingent? Do courts invoke the charter''s principles in modern cases? Can the restraint language be separated from its feudal context and applied to modern sovereignty structures?',
    'If the restraint language is universal, the feudal obsolescence reading is false—the charter would be binding as inherited constitutional principle. If the restraint language is genuinely feudal-specific, the reading is correct and modern sovereigns face no Magna Carta constraint. If the language is partially separable (some provisions universal, some feudal-specific), the constraint''s type shifts: it becomes tangled rope (mixed coordination and extraction) or snare (extraction riding on a false historical claim that medieval law is inapplicable).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(feudal_vs_universal_restraint, conceptual, 'Whether Magna Carta''s restraint principles are historically contingent or universally binding.').

omega_variable(
    parliamentary_vs_executive_reading,
    'Does parliamentary statute law supersede Magna Carta''s restraint, or does the charter persist as a constitutional restraint on Parliament itself?',
    'Historical analysis of parliamentary incorporation: did Parliament codify Magna Carta as binding statute, or did Parliament treat it as a historical artifact? Can Parliament repeal Magna Carta''s restraints without constitutional consequence, or do the restraints persist as fundamental law that Parliament cannot alter?',
    'If Parliament genuinely superseded and can repeal Magna Carta, the feudal obsolescence reading is strengthened—the charter is subordinate to statute and has no independent constitutional force. If the charter persists as fundamental law that Parliament cannot alter, the reading is weakened—restraint persists through a successor mechanism (parliamentary constitutionalism) and the constraint is not a piton but a transformed rope or tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(parliamentary_vs_executive_reading, empirical, 'Whether parliamentary statute law makes Magna Carta obsolete or incorporates its restraints as binding.').

omega_variable(
    doctrine_vs_practice_gap,
    'Does the feudal obsolescence reading reflect actual judicial and executive practice, or is it a doctrinal claim that masks ongoing implicit constraint from Magna Carta''s restraint language?',
    'Empirical analysis of court decisions and executive action: how often do courts and executives invoke Magna Carta''s restraint language, even while claiming it is historically obsolete? Do they constrain themselves as if the charter were binding, while maintaining the doctrinal position that it is not?',
    'If courts and executives operate as if the charter were binding (constraining themselves by its language), the reading is performative—a doctrinal claim maintained for institutional autonomy while actual constraint persists. If they operate without reference to the charter''s restraint language, the reading is practically true. The gap between doctrine and practice determines whether the constraint is a snare (false claim of obsolescence enabling hidden extraction) or a piton (genuinely atrophied restraint maintained theatrically).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(doctrine_vs_practice_gap, empirical, 'Whether the feudal obsolescence reading masks implicit constraint from Magna Carta''s language.').

omega_variable(
    committer_kernel_contest,
    'This constraint is one reading of the contested Magna Carta kernel. The feudal obsolescence reading coexists with living constitutionalism and parliamentary sovereignty readings held by different institutional and intellectual seats. Which reading captures the constraint''s actual binding force: history, evolutionary interpretation, or parliamentary statute?',
    'The three readings are live commitments held by different parties in ongoing constitutional dispute. No neutral arbiter can settle which reading is ''correct''—each reading instantiates a different constraint (different ε, different beneficiary/victim structure, different type) and the corpus measures all three as separate stories. The mismatch between readings is not an omega to resolve but a structural fact the corpus documents.',
    'This omega documents that the feudal obsolescence reading is chosen from among live alternatives, not discovered as a historical fact. The choice shapes which actors benefit (executive authority benefits from obsolescence framing; popular constitutionalists benefit from living-tradition framing) and which restraints persist (or are claimed to persist). The engine computes per-seat classification, which will diverge across the three readings—that divergence is diagnostic of the kernel contest, not a defect.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_kernel_contest, conceptual, 'Documentation that this reading is one choice among live constitutional alternatives; the kernel contest is structural, not resolvable.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magna_carta_constraint_authority__feudal_obsolescence_reading, 1215, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(magn_tr_t1215, magna_carta_constraint_authority__feudal_obsolescence_reading, theater_ratio, 1215, 0.15).
narrative_ontology:measurement(magn_tr_t1485, magna_carta_constraint_authority__feudal_obsolescence_reading, theater_ratio, 1485, 0.25).
narrative_ontology:measurement(magn_tr_t1689, magna_carta_constraint_authority__feudal_obsolescence_reading, theater_ratio, 1689, 0.35).
narrative_ontology:measurement(magn_tr_t1832, magna_carta_constraint_authority__feudal_obsolescence_reading, theater_ratio, 1832, 0.45).
narrative_ontology:measurement(magn_tr_t1945, magna_carta_constraint_authority__feudal_obsolescence_reading, theater_ratio, 1945, 0.65).
narrative_ontology:measurement(magn_tr_t1997, magna_carta_constraint_authority__feudal_obsolescence_reading, theater_ratio, 1997, 0.72).
narrative_ontology:measurement(magn_tr_t2026, magna_carta_constraint_authority__feudal_obsolescence_reading, theater_ratio, 2026, 0.71).

% Extraction over time
narrative_ontology:measurement(magn_be_t1215, magna_carta_constraint_authority__feudal_obsolescence_reading, base_extractiveness, 1215, 0.08).
narrative_ontology:measurement(magn_be_t1485, magna_carta_constraint_authority__feudal_obsolescence_reading, base_extractiveness, 1485, 0.12).
narrative_ontology:measurement(magn_be_t1689, magna_carta_constraint_authority__feudal_obsolescence_reading, base_extractiveness, 1689, 0.18).
narrative_ontology:measurement(magn_be_t1832, magna_carta_constraint_authority__feudal_obsolescence_reading, base_extractiveness, 1832, 0.28).
narrative_ontology:measurement(magn_be_t1945, magna_carta_constraint_authority__feudal_obsolescence_reading, base_extractiveness, 1945, 0.51).
narrative_ontology:measurement(magn_be_t1997, magna_carta_constraint_authority__feudal_obsolescence_reading, base_extractiveness, 1997, 0.59).
narrative_ontology:measurement(magn_be_t2026, magna_carta_constraint_authority__feudal_obsolescence_reading, base_extractiveness, 2026, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(magn_su_t1215, magna_carta_constraint_authority__feudal_obsolescence_reading, suppression_requirement, 1215, 0.22).
narrative_ontology:measurement(magn_su_t1485, magna_carta_constraint_authority__feudal_obsolescence_reading, suppression_requirement, 1485, 0.28).
narrative_ontology:measurement(magn_su_t1689, magna_carta_constraint_authority__feudal_obsolescence_reading, suppression_requirement, 1689, 0.35).
narrative_ontology:measurement(magn_su_t1832, magna_carta_constraint_authority__feudal_obsolescence_reading, suppression_requirement, 1832, 0.42).
narrative_ontology:measurement(magn_su_t1945, magna_carta_constraint_authority__feudal_obsolescence_reading, suppression_requirement, 1945, 0.54).
narrative_ontology:measurement(magn_su_t1997, magna_carta_constraint_authority__feudal_obsolescence_reading, suppression_requirement, 1997, 0.57).
narrative_ontology:measurement(magn_su_t2026, magna_carta_constraint_authority__feudal_obsolescence_reading, suppression_requirement, 2026, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(magna_carta_constraint_authority__feudal_obsolescence_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(magna_carta_constraint_authority__feudal_obsolescence_reading, 0.12).
narrative_ontology:affects_constraint(magna_carta_constraint_authority__feudal_obsolescence_reading, magna_carta_constraint_authority__living_constitutionalism_reading).
narrative_ontology:affects_constraint(magna_carta_constraint_authority__feudal_obsolescence_reading, magna_carta_constraint_authority__parliamentary_sovereignty_reading).
narrative_ontology:affects_constraint(magna_carta_constraint_authority__feudal_obsolescence_reading, english_bill_of_rights_constraint_authority).
narrative_ontology:affects_constraint(magna_carta_constraint_authority__feudal_obsolescence_reading, universal_human_rights_due_process_constraint).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested Magna Carta kernel. The feudal obsolescence reading treats the charter as historically inert and non-binding on modern sovereigns. The living constitutionalism reading treats the charter's restraint language as binding through evolutionary interpretation. The parliamentary sovereignty reading treats the charter's restraints as surviving only through parliamentary statute. All three readings share the same kernel (Magna Carta's constitutional authority) but instantiate different constraints (different ε, different type, different beneficiary/victim structure). The network edges link the readings and their downstream constraints (English Bill of Rights, modern human rights law) to document the constraint family and facilitate cross-reading comparison.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
