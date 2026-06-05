% ============================================================================
% CONSTRAINT STORY: second_amendment_text__collective_rights_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_text__collective_rights_reading, []).

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
 *   constraint_id: second_amendment_text__collective_rights_reading
 *   human_readable: Second Amendment as Collective Militia Authority (Textual Reading)
 *   domain: constitutional_law/political_philosophy/firearms_policy
 *
 * SUMMARY:
 *   The collective-rights reading of the Second Amendment interprets the text
 *   as granting authority to states to regulate private arms while preserving
 *   militia function. This reading emerged as a dominant legal interpretation
 *   for most of the 20th century (until District of Columbia v. Heller in
 *   2008 shifted majority doctrine toward individual rights). The collective
 *   reading creates a tangled coordination-extraction structure: it
 *   coordinates state militia authority and public safety governance
 *   (beneficiary perspective) while suppressing individual ownership claims
 *   and constraining firearms commerce (victim perspective). The constraint
 *   exhibits high theater ratio because contemporary scholarly defenses of
 *   the collective reading increasingly rely on disputed historical claims
 *   about 'original public meaning' that cannot be decisively resolved from
 *   surviving evidence. The suppression requirement (0.68) reflects that
 *   enforcing this reading against the individual-rights interpretation
 *   requires sustained institutional work—doctrinal redefinition, precedent
 *   distinguishing, and framing control—because the operative clause's plain
 *   language supports the individual reading more naturally. The
 *   extractiveness trajectory (0.35→0.52 over 80 years) shows accumulation:
 *   the collective reading was more stable when it faced no serious
 *   constitutional challenge (1920–2000), but as the individual-rights
 *   interpretation gained institutional traction post-Heller, the collective
 *   reading's suppressive force intensified, requiring more sophisticated
 *   doctrinal work to maintain its claim to textual authenticity.
 *
 * KEY AGENTS:
 *   - State Regulatory Apparatus: Primary beneficiary (institutional/arbitrage) — gains plenary authority to regulate private arms; militia authority flows to state control
 *   - Public Safety Governance: Primary beneficiary (institutional/arbitrage) — law enforcement and public health authorities gain constitutional warrant for arms regulation
 *   - Individual Gun Owners: Primary victim (powerless/trapped) — under strict collective reading, no constitutional protection for private ownership; exit is illegal
 *   - Firearms Manufacturing & Commerce: Secondary victim (moderate/constrained) — market is constrained to state-regulated militia procurement and possibly licensed domestic use; exit through relocation or policy advocacy
 *   - Second Amendment Advocacy Coalition (Collective Interpreters): Organized defender (organized/constrained) — maintains collective-rights reading against rival interpretation; constrained by need to answer individual-rights scholarship
 *   - Historical-Textual Scholarly Apparatus: Analytical observer (analytical/analytical) — claims to recover founding intent through historical evidence; increasingly performs theatrical work as evidence remains ambiguous
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_text__collective_rights_reading, 0.52).
domain_priors:suppression_score(second_amendment_text__collective_rights_reading, 0.68).
domain_priors:theater_ratio(second_amendment_text__collective_rights_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_text__collective_rights_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(second_amendment_text__collective_rights_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(second_amendment_text__collective_rights_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_text__collective_rights_reading, tangled_rope).
narrative_ontology:human_readable(second_amendment_text__collective_rights_reading, "Second Amendment as Collective Militia Authority (Textual Reading)").
narrative_ontology:topic_domain(second_amendment_text__collective_rights_reading, "constitutional_law/political_philosophy/firearms_policy").

domain_priors:requires_active_enforcement(second_amendment_text__collective_rights_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_text__collective_rights_reading, 'b848041a-e8a7-4466-ac6a-78e3a7e7eb3a').
narrative_ontology:cs_kernel_codification('b848041a-e8a7-4466-ac6a-78e3a7e7eb3a', fixed_text).
narrative_ontology:cs_authority_grounding('b848041a-e8a7-4466-ac6a-78e3a7e7eb3a', lineage).
narrative_ontology:cs_interpretation_layer_present('b848041a-e8a7-4466-ac6a-78e3a7e7eb3a').
narrative_ontology:cs_reading_relation('b848041a-e8a7-4466-ac6a-78e3a7e7eb3a', second_amendment_text__individual_rights_reading, coexists_with).
narrative_ontology:cs_reading_relation('b848041a-e8a7-4466-ac6a-78e3a7e7eb3a', second_amendment_text__hybrid_civic_reading, coexists_with).
narrative_ontology:cs_axiom('b848041a-e8a7-4466-ac6a-78e3a7e7eb3a', foundational, militia_service_bounds_ownership_right).
narrative_ontology:cs_axiom_status(militia_service_bounds_ownership_right, holdable).
narrative_ontology:cs_axiom_grounding('b848041a-e8a7-4466-ac6a-78e3a7e7eb3a', militia_service_bounds_ownership_right, conventional).
narrative_ontology:cs_axiom('b848041a-e8a7-4466-ac6a-78e3a7e7eb3a', secondary, prefatory_clause_restricts_operative_scope).
narrative_ontology:cs_axiom_status(prefatory_clause_restricts_operative_scope, holdable).
narrative_ontology:cs_axiom_grounding('b848041a-e8a7-4466-ac6a-78e3a7e7eb3a', prefatory_clause_restricts_operative_scope, empirically_contingent).
narrative_ontology:cs_reference_frame('b848041a-e8a7-4466-ac6a-78e3a7e7eb3a', federalist_militia_authority).
narrative_ontology:cs_drift_state('b848041a-e8a7-4466-ac6a-78e3a7e7eb3a', contemporary_supreme_court_doctrine, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('b848041a-e8a7-4466-ac6a-78e3a7e7eb3a', '').
narrative_ontology:cs_kernel_id(second_amendment_text__collective_rights_reading, second_amendment_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_text__collective_rights_reading, state_regulatory_apparatus).
narrative_ontology:constraint_beneficiary(second_amendment_text__collective_rights_reading, public_safety_governance).
narrative_ontology:constraint_victim(second_amendment_text__collective_rights_reading, individual_gun_owners).
narrative_ontology:constraint_victim(second_amendment_text__collective_rights_reading, firearms_industry).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDIVIDUAL GUN OWNERS (SNARE) — Under the collective-rights reading, private citizens have no constitutionally protected claim to individual ownership. Exit is legally impossible within this interpretive frame. Suppression is maximal: state regulatory apparatus can restrict individual ownership entirely while framing it as constitutional. The individual experiences this as pure extraction with no escape clause.
constraint_indexing:constraint_classification(second_amendment_text__collective_rights_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: FIREARMS INDUSTRY (TANGLED ROPE) — The collective-rights reading constrains private commercial arms production but permits state-regulated militia arms procurement. Industry has constrained exit: it can lobby for interpretive shift or relocate regulatory jurisdiction, but the constitutional frame itself limits market size. Industry also benefits from militia demand and from state security contracting. Mixed extraction and coordination — genuine demand for regulated arms exists alongside commercial suppression.
constraint_indexing:constraint_classification(second_amendment_text__collective_rights_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: STATE REGULATORY APPARATUS (ROPE) — The collective-rights reading directly benefits state actors. It grants plenary authority to regulate private arms while preserving the militia function. The state experiences this as pure coordination: organizing the regulated militia serves collective defense while constraining the armed challenge to state authority. Arbitrage exit means the state can shift institutional arrangements while maintaining core authority. Net beneficiary perspective.
constraint_indexing:constraint_classification(second_amendment_text__collective_rights_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: PUBLIC SAFETY GOVERNANCE (ROPE) — Law enforcement and public health authorities benefit from the collective-rights reading's authorization of arms regulation. No tension between constitutional protection and safety policy. The constraint functions as pure coordination: the reading aligns constitutional interpretation with regulatory authority over dangerous goods. Arbitrage exit available through policy reframing without constitutional challenge.
constraint_indexing:constraint_classification(second_amendment_text__collective_rights_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: SECOND AMENDMENT ADVOCACY COALITION—COLLECTIVE READING (TANGLED ROPE) — Organized actors committed to the collective-rights interpretation derive status and institutional position from its academic and legal elaboration. They experience this as mixed: the reading privileges state authority (coordination function) but faces sustained constitutional challenge from organized rival interpreters (extraction/suppression from the individual-rights coalition). Their exit is constrained by the need to maintain doctrinal coherence against challenge.
constraint_indexing:constraint_classification(second_amendment_text__collective_rights_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: HISTORICAL-TEXTUAL ANALYSIS (PITON) — The scholarly apparatus that claims to demonstrate the collective-rights reading from historical evidence (founding-era militia practice, prefatory clause syntax, early state constitutions) increasingly performs theatrical work. Modern historical consensus shows genuine ambiguity: founding sources support both militia-focused and individual-ownership readings simultaneously; the 'original public meaning' is not determinable from available evidence. Piton classification: the theater ratio reflects that contemporary scholarly claims of historical certainty rest on curated evidence and contested interpretive methods, not on decisively recovered intent.
constraint_indexing:constraint_classification(second_amendment_text__collective_rights_reading, piton,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_text__collective_rights_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(second_amendment_text__collective_rights_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(second_amendment_text__collective_rights_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(second_amendment_text__collective_rights_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(second_amendment_text__collective_rights_reading, TR),
    TR >= 0.70.

:- end_tests(second_amendment_text__collective_rights_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The collective reading structures extraction in two directions simultaneously: (1) toward the state (beneficiary), which gains regulatory authority and suppresses individual ownership claims; (2) from the firearms industry, whose market is constrained. For individual owners, this is maximal extraction—the reading denies them constitutional protection. For the industry, it's moderate—constrained but not eliminated (state militia procurement and some regulated civilian use remain). The moderate-high average reflects that the reading is genuinely mixed: it serves coordination (militia organization, public safety) alongside extraction (individual suppression). Suppression (0.68): High. Enforcing the collective reading requires suppressing the plain language of the operative clause ('the right of the people...shall not be infringed'), which more naturally reads as individual protection. It also requires suppressing the scholarly consensus that founding-era Americans understood 'the people's right to bear arms' to include individual ownership for defense purposes. High suppression indicates the reading's enforcement depends on doctrinal authority and framing control, not on transparent textual support. Theater ratio (0.58): Moderate-high. Contemporary defenses of the collective reading claim to derive it from 'original public meaning' and founding-era militia practice, but these historical claims are increasingly contested. The scholarly work of establishing the collective reading's authenticity has shifted from historical documentation (which was never decisive) to rhetorical framing—emphasizing prefatory clause syntax, militia-specific contexts, and selective historical examples. The theater has grown because the reading faces a well-articulated rival interpretation with substantial institutional backing (post-Heller individual-rights doctrine).
 *
 * PERSPECTIVAL GAP:
 *   The collective-rights reading produces maximum perspectival divergence. Individual gun owners see a snare—no constitutional protection, no exit, maximal suppression. The firearms industry sees tangled rope—constrained but not eliminated, with some benefits from militia demand. State actors see rope—pure coordination of militia authority with no perceived extraction. Public safety governance sees rope—alignment with regulatory authority. Organized collective-rights interpreters see tangled rope—they coordinate state authority but face suppression from rival interpreters. Scholarly analysis sees piton—the reading's historical claims are increasingly performative because the evidence genuinely supports both readings simultaneously. The perspectival gaps reveal that this reading's stability depends on institutional power (state authority, doctrinal precedent) rather than on textual clarity or historical certainty.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality (d) derives from structural position relative to the constraint. Individual owners: victim status + trapped exit → d ≈ 0.95, f(d) ≈ 1.42, high χ. State apparatus: beneficiary status + arbitrage exit → d ≈ 0.05, f(d) ≈ -0.12, negative χ (institutional benefit). Firearms industry: victim status + constrained exit → d ≈ 0.70, f(d) ≈ 0.95, moderate χ. Public safety: beneficiary status + arbitrage exit → d ≈ 0.10, f(d) ≈ -0.05, low/negative χ. Collective interpreters: mixed (beneficiary of doctrinal authority but victim of rival interpretation) + constrained exit → d ≈ 0.50, f(d) ≈ 0.65, moderate χ. The analytical observer: d ≈ 0.72 (canonical for analytical), f(d) ≈ 1.15, moderate χ, but sees through to the theater underlying the reading's claimed authenticity.
 *
 * MANDATROPHY ANALYSIS:
 *   This collective-rights reading resolves mandatrophy by declaring genuine beneficiaries (state apparatus, public safety governance) and genuine coordination function (militia organization, arms regulation) alongside genuine extraction (suppression of individual ownership, constrained industry). The reading is not pure extraction masquerading as coordination (that would be a snare) nor pure coordination masquerading as inevitable law (that would be a false summit). Rather, it is genuinely tangled: it coordinates state authority and serves public safety while simultaneously extracting from individual owners and constraining the firearms industry. The extraction component is not hidden—it is explicit in the reading's restriction of private ownership. The coordination component is real—militia function and public safety governance are genuine coordination problems. Mandatrophy is resolved by accepting both: this reading serves real coordination functions (hence not snare) while imposing real asymmetric costs (hence not rope).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    prefatory_clause_binding_force,
    'Does the prefatory clause (''A well regulated Militia, being necessary to the security of a free State'') restrict the operative clause''s scope, or merely provide rationale without limiting application?',
    'Comparative constitutional syntax analysis: how do prefatory clauses function in other constitutions (founding-era state constitutions, international instruments)? Do prefatory clauses universally restrict scope or merely provide context?',
    'If prefatory clause is binding: collective-rights reading is structurally sound (right derives from militia service). If prefatory is non-restrictive: operative clause (''the right of the people...shall not be infringed'') independently protects individual ownership; collective reading requires additional suppression of plain language.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(prefatory_clause_binding_force, empirical, 'Whether prefatory clause syntactically restricts the operative clause''s scope').

omega_variable(
    founding_intent_determinability,
    'Is the founding-era understanding of ''the people''s right to bear arms'' determinably militia-collective or individually-protective, given surviving documentary evidence?',
    'Systematic analysis of founding-era texts (ratification debates, state constitutions, militia statutes, private correspondence). Identification of explicit statements distinguishing militia service from private ownership claims. Assessment of whether evidence is sufficient to resolve ambiguity or merely illustrates both readings'' plausibility.',
    'If collective intent is demonstrable: the collective-rights reading has high textual warrant. If intent is genuinely ambiguous: both readings remain plausible and the choice between them is principally about which framework better serves contemporary governance values.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(founding_intent_determinability, empirical, 'Whether founding-era intent favors collective-militia or individual-ownership reading').

omega_variable(
    alternative_reading_coexistence,
    'Can the collective-rights reading coexist within a single constitutional framework with the individual-rights reading, or must one reading foreclose the other?',
    'Logical analysis: would a framework that permits both readings simultaneously create internal contradiction, or merely distribute authority across different spheres (militia regulation vs individual ownership)? Examination of hybrid readings (e.g., individual right to own arms for militia purposes, but not all purposes).',
    'If readings foreclose each other: the constraint is fundamentally about which interpretation wins and suppresses its rival (Snare dynamics at the doctrine level). If readings can coexist: the constraint is about boundary maintenance between jurisdictions (Tangled Rope coordination with asymmetric power).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_reading_coexistence, conceptual, 'Whether collective and individual readings logically foreclose each other or can coexist').

omega_variable(
    militia_concept_historical_fixity,
    'Does ''militia'' in the founding era have a fixed, transparent meaning, or does it refer to an evolving institutional concept?',
    'Comparative analysis of militia law across founding-era states and the subsequent evolution of militia institutions (transformation from universal male service obligation to National Guard professionalization). Assessment of whether the collective-rights reading is anchored to a specific historical-moment definition or must evolve with institutional change.',
    'If militia is historically fixed: collective reading is stable and its boundaries determinable. If militia is institutionally mutable: the collective reading must either narrow (apply only to historical militia configurations) or expand (apply to evolved militia institutions), risking loss of constraining force.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(militia_concept_historical_fixity, empirical, 'Whether ''militia'' has fixed or evolving historical meaning').

omega_variable(
    kernel_reading_foreclosure_mutual,
    'Is this collective-rights reading a genuinely distinct kernel interpretation, or is it a weaker variant of a hybrid reading that the individual-rights reading has already partly foreclosed?',
    'Examination of whether Supreme Court precedent (especially District of Columbia v. Heller 2008) has foreclosed the pure collective reading''s doctrinal coherence. Assessment of whether the reading can function as a primary legal authority or only as a secondary argument within hybrid frameworks.',
    'If foreclosed by precedent: this reading''s extractive force derives from suppressing acknowledged precedent rather than from constitutional text itself (shifts classification toward Snare). If still live: reading retains doctrinal coherence despite unfavorable precedent.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure_mutual, conceptual, 'Whether collective reading is foreclosed by Supreme Court precedent or remains live').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_text__collective_rights_reading, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sa2a_collective_tr_t0, second_amendment_text__collective_rights_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement(sa2a_collective_tr_t40, second_amendment_text__collective_rights_reading, theater_ratio, 40, 0.52).
narrative_ontology:measurement(sa2a_collective_tr_t80, second_amendment_text__collective_rights_reading, theater_ratio, 80, 0.58).

% Extraction over time
narrative_ontology:measurement(sa2a_collective_be_t0, second_amendment_text__collective_rights_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(sa2a_collective_be_t40, second_amendment_text__collective_rights_reading, base_extractiveness, 40, 0.44).
narrative_ontology:measurement(sa2a_collective_be_t80, second_amendment_text__collective_rights_reading, base_extractiveness, 80, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(sa2a_collective_su_t0, second_amendment_text__collective_rights_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(sa2a_collective_su_t40, second_amendment_text__collective_rights_reading, suppression_requirement, 40, 0.62).
narrative_ontology:measurement(sa2a_collective_su_t80, second_amendment_text__collective_rights_reading, suppression_requirement, 80, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_text__collective_rights_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(second_amendment_text__collective_rights_reading, second_amendment_text__individual_rights_reading).
narrative_ontology:affects_constraint(second_amendment_text__collective_rights_reading, second_amendment_text__hybrid_civic_reading).
narrative_ontology:affects_constraint(second_amendment_text__collective_rights_reading, firearms_regulation_commerce_clause).
narrative_ontology:affects_constraint(second_amendment_text__collective_rights_reading, state_militia_authority_supremacy).

% DUAL FORMULATION NOTE:
% The Second Amendment text constraint family decomposes into three reading-specific stories corresponding to distinct interpretive frameworks. Each reading has different epsilon values reflecting different empirical assumptions about historical meaning and different beneficiary/victim structures reflecting different constitutional distributions of power. The collective-rights reading (this file) has ε=0.52 and benefits state apparatus; the individual-rights reading (sibling) has higher ε (~0.65) reflecting greater suppression of public safety arguments; the hybrid-civic reading (sibling) has intermediate ε (~0.45) reflecting balance between both readings' coordination functions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
