% ============================================================================
% CONSTRAINT STORY: qualified_immunity_doctrine__constitutional_fidelity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_qualified_immunity_doctrine__constitutional_fidelity_reading, []).

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
 *   constraint_id: qualified_immunity_doctrine__constitutional_fidelity_reading
 *   human_readable: Qualified Immunity as Judicially Fabricated Doctrine (Constitutional Fidelity Reading)
 *   domain: constitutional_law/civil_rights/law_enforcement_policy
 *
 * SUMMARY:
 *   Qualified immunity is a judicially created doctrine that shields law
 *   enforcement and other officials from civil liability if their conduct did
 *   not violate a 'clearly established' constitutional right at the time of
 *   the alleged violation. This reading treats the doctrine as illegitimate
 *   regardless of its policy consequences because it lacks constitutional or
 *   statutory authorization. The doctrine emerged from judicial
 *   interpretation (Pierson v. Ray, 1967; Harlow v. Fitzgerald, 1982) without
 *   textual foundation in the Constitution or in 42 USC 1983 (the primary
 *   statute enabling civil rights suits against officials). The
 *   constitutional fidelity reading frames this as a structural illegitimacy:
 *   the judiciary exceeded its authority by creating a substantive immunity
 *   rule that Congress did not authorize and the Constitution does not
 *   require. Unlike the protective_scaffold_reading (which views immunity as
 *   necessary temporary protection) or the accountability_void_reading (which
 *   focuses on the empirical failure of the doctrine to balance interests),
 *   this reading denies the legitimacy of the entire framework regardless of
 *   whether it produces good policy outcomes. The constraint exhibits snare
 *   characteristics: civil rights claimants are trapped by a doctrine that
 *   offers no constitutional or statutory escape; the beneficiaries
 *   (judiciary, law enforcement) are institutional actors who experience the
 *   doctrine as coordination (case filtering, operational protection); and
 *   the suppression is high because there is no legitimate alternative remedy
 *   recognized by the court system.
 *
 * KEY AGENTS:
 *   - Civil Rights Claimants: Primary victims (powerless/trapped) — face complete remedy denial under a doctrine they cannot exit
 *   - Lower Court Judges: Secondary victims (moderate/constrained) — bound by Supreme Court precedent, cannot enforce constitutional accountability
 *   - Supreme Court Judiciary: Primary beneficiary (institutional/arbitrage) — derives institutional efficiency and authority expansion from doctrine maintenance
 *   - Law Enforcement Agencies: Institutional beneficiary (powerful/mobile) — benefits from operationally protective immunity despite lack of constitutional/statutory warrant
 *   - Civil Rights Organizations: Secondary actor (powerful/constrained) — extract mobilization benefit from doctrine's focal clarity but suffer from remedy denial for constituents
 *   - Constitutional Accountability Framework: Abstract victim (powerless/trapped) — the system itself is damaged by lack of judicial enforcement of constitutional rights
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qualified_immunity_doctrine__constitutional_fidelity_reading, 0.68).
domain_priors:suppression_score(qualified_immunity_doctrine__constitutional_fidelity_reading, 0.72).
domain_priors:theater_ratio(qualified_immunity_doctrine__constitutional_fidelity_reading, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qualified_immunity_doctrine__constitutional_fidelity_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(qualified_immunity_doctrine__constitutional_fidelity_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(qualified_immunity_doctrine__constitutional_fidelity_reading, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qualified_immunity_doctrine__constitutional_fidelity_reading, snare).
narrative_ontology:human_readable(qualified_immunity_doctrine__constitutional_fidelity_reading, "Qualified Immunity as Judicially Fabricated Doctrine (Constitutional Fidelity Reading)").
narrative_ontology:topic_domain(qualified_immunity_doctrine__constitutional_fidelity_reading, "constitutional_law/civil_rights/law_enforcement_policy").

domain_priors:requires_active_enforcement(qualified_immunity_doctrine__constitutional_fidelity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qualified_immunity_doctrine__constitutional_fidelity_reading, 'dc654683-0bf7-444b-9a9a-05bb2883ee52').
narrative_ontology:cs_kernel_codification('dc654683-0bf7-444b-9a9a-05bb2883ee52', formalized).
narrative_ontology:cs_authority_grounding('dc654683-0bf7-444b-9a9a-05bb2883ee52', extraction).
narrative_ontology:cs_interpretation_layer_present('dc654683-0bf7-444b-9a9a-05bb2883ee52').
narrative_ontology:cs_reading_relation('dc654683-0bf7-444b-9a9a-05bb2883ee52', qualified_immunity_doctrine__protective_scaffold_reading, coexists_with).
narrative_ontology:cs_reading_relation('dc654683-0bf7-444b-9a9a-05bb2883ee52', qualified_immunity_doctrine__accountability_void_reading, coexists_with).
narrative_ontology:cs_axiom('dc654683-0bf7-444b-9a9a-05bb2883ee52', foundational, judicial_fabrication_illegitimate).
narrative_ontology:cs_axiom_status(judicial_fabrication_illegitimate, holdable).
narrative_ontology:cs_axiom_grounding('dc654683-0bf7-444b-9a9a-05bb2883ee52', judicial_fabrication_illegitimate, deontological).
narrative_ontology:cs_axiom('dc654683-0bf7-444b-9a9a-05bb2883ee52', foundational, statutory_authorization_required).
narrative_ontology:cs_axiom_status(statutory_authorization_required, holdable).
narrative_ontology:cs_axiom_grounding('dc654683-0bf7-444b-9a9a-05bb2883ee52', statutory_authorization_required, deontological).
narrative_ontology:cs_reference_frame('dc654683-0bf7-444b-9a9a-05bb2883ee52', constitutional_accountability_framework).
narrative_ontology:cs_drift_state('dc654683-0bf7-444b-9a9a-05bb2883ee52', contemporary_expanded_immunity_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('dc654683-0bf7-444b-9a9a-05bb2883ee52', '').
narrative_ontology:cs_kernel_id(qualified_immunity_doctrine__constitutional_fidelity_reading, qualified_immunity_doctrine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(qualified_immunity_doctrine__constitutional_fidelity_reading, judiciary).
narrative_ontology:constraint_beneficiary(qualified_immunity_doctrine__constitutional_fidelity_reading, law_enforcement_agencies).
narrative_ontology:constraint_victim(qualified_immunity_doctrine__constitutional_fidelity_reading, civil_rights_claimants).
narrative_ontology:constraint_victim(qualified_immunity_doctrine__constitutional_fidelity_reading, constitutional_accountability_framework).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CIVIL RIGHTS CLAIMANT (SNARE) — Trapped by judicial doctrine that is neither constitutional nor statutory. No legitimate legal recourse against officials who violate rights. The claimant faces maximum suppression (no alternative remedial pathway) and bears full extraction cost (remedy denied despite constitutional injury). Exit is structural impossibility — the claimant cannot exit the jurisdiction or the Constitution.
constraint_indexing:constraint_classification(qualified_immunity_doctrine__constitutional_fidelity_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: LOWER COURT JUDGE (SNARE) — Bound by Supreme Court precedent establishing qualified immunity. Cannot exit the doctrine without overruling Supreme Court authority. Suppression manifests as institutional constraint: following Supreme Court doctrine is non-negotiable. Experiences the doctrine as extraction because it prevents judges from enforcing constitutional accountability.
constraint_indexing:constraint_classification(qualified_immunity_doctrine__constitutional_fidelity_reading, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: SUPREME COURT JUDICIARY (ROPE) — From the institutional perspective, qualified immunity coordinates judicial authority expansion and case-filtering efficiency. Courts experience the doctrine as coordination: it enables rapid dismissal of civil rights claims, reducing docket burden. Institutional beneficiary with arbitrage capacity — can revise doctrine, but currently derives significant functional benefit from maintaining it. The coordination function is institutional self-preservation (filtering cases, reducing workload).
constraint_indexing:constraint_classification(qualified_immunity_doctrine__constitutional_fidelity_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: LAW ENFORCEMENT AGENCY (ROPE) — Experiences qualified immunity as coordination: officers need stable legal rules to operate without paralyzing fear of suit. From this perspective, the doctrine solves a genuine collective action problem (enabling officers to act decisively). However, this reading (constitutional fidelity) denies the legitimacy of that coordination function — the coordination is built on an illegitimate foundation. Powerful actor with mobile exit options (can lobby for statutory replacement, can change protocols), but currently benefits from doctrine maintenance.
constraint_indexing:constraint_classification(qualified_immunity_doctrine__constitutional_fidelity_reading, rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: CIVIL RIGHTS ORGANIZATIONS (TANGLED ROPE) — Organized agents with mixed structural relationship. The doctrine extracts (denies remedies to their constituents). But organizations also benefit from the doctrine's prominence as a focal point for advocacy and mobilization — qualified immunity is a clear, recognizable target in ways that more diffuse immunity doctrines would not be. Constrained exit (reform requires constitutional amendment or complete Supreme Court reversal) but genuine coordinating function exists (identifies the specific locus of illegitimate immunity).
constraint_indexing:constraint_classification(qualified_immunity_doctrine__constitutional_fidelity_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: CONSTITUTIONAL DOCTRINE AS INSTITUTIONAL ARTIFACT (PITON) — From the longest time horizon, qualified immunity persists through institutional inertia and theater despite lacking constitutional or statutory foundation. The doctrine maintains itself through case citation, integration into civil procedure, and institutionalization in police training and municipal liability law. Theater ratio is high because much of the doctrine's justificatory apparatus (public official immunity tradition, balancing tests) is performative — the core claim is that judicial creation is illegitimate, making all justifications theater. The doctrine is a vestige that persists through institutional momentum and path dependence rather than legitimate authority.
constraint_indexing:constraint_classification(qualified_immunity_doctrine__constitutional_fidelity_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From the purely structural perspective, some immunity for public officials may be a natural law of governance: officials who must make split-second decisions in public interest require some insulation from permanent liability exposure. This perspective treats qualified immunity as an unavoidable structural feature of any delegation-of-authority system, not a contingent doctrine. However, this reading (constitutional fidelity) denies the mountain classification — it asserts the illegitimacy of ANY non-constitutional shield. The engine will identify this as a false summit: the 'natural law' framing naturalizes what the constitutional fidelity reading treats as illegitimate doctrine.
constraint_indexing:constraint_classification(qualified_immunity_doctrine__constitutional_fidelity_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(qualified_immunity_doctrine__constitutional_fidelity_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(qualified_immunity_doctrine__constitutional_fidelity_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(qualified_immunity_doctrine__constitutional_fidelity_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(qualified_immunity_doctrine__constitutional_fidelity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(qualified_immunity_doctrine__constitutional_fidelity_reading, TR),
    TR >= 0.70.

:- end_tests(qualified_immunity_doctrine__constitutional_fidelity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. From this reading's perspective, the doctrine extracts from civil rights claimants by denying them legitimate remedies grounded in the Constitution or statute. The extraction is measured as the gap between what constitutional accountability would require (judicial enforcement of constitutional rights) and what qualified immunity permits (dismissal if rights were not clearly established). The measurement shows increasing extractiveness over the 30-year interval (0.52 to 0.68), reflecting scope creep as the doctrine has expanded through case law to shield broader categories of conduct. Suppression (0.72): High. Suppression is structural: there is no legitimate alternative remedy available through federal courts because qualified immunity is itself the framework that governs Section 1983 suits. The suppression has increased over time (0.58 to 0.72) as judges have made the 'clearly established' standard increasingly difficult for claimants to satisfy, creating redundant barriers. Theater ratio (0.65): Moderate-high. The justificatory apparatus surrounding qualified immunity — the balancing of public official efficiency against individual rights protection, the clearly established law test, the summary judgment practice — is performative from this reading's perspective because the entire framework lacks constitutional/statutory legitimacy. The theater has increased (0.45 to 0.65) as the doctrine has elaborated its justificatory stories without addressing the foundational illegitimacy claim. The reading does not deny that officials need some protection from suit paralyzing; it denies that the judiciary had authority to create that protection absent constitutional text or Congressional authorization.
 *
 * PERSPECTIVAL GAP:
 *   This reading generates maximum perspectival divergence. Civil rights claimants see pure snare (complete remedy denial). Lower court judges see snare (institutional constraint preventing constitutional enforcement). The Supreme Court sees rope (institutional coordination of efficiency and authority). Law enforcement sees rope (operational protection enabling agency). Civil rights organizations see tangled rope (mixed extraction and mobilization benefit). Institutional doctrine itself appears as piton (performative maintenance through institutional inertia). The analytical observer risks seeing mountain (immunity as necessary feature of any delegation system), but this reading rejects that naturalization. The perspectival gaps reveal fundamentally incompatible framings: one group sees legitimacy crisis, another sees necessary coordination, another sees empirical failure. The divergence is not perspectival ambiguity but genuine structural conflict between readings.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) are derived from the agent's structural relationship to the extraction flow defined by this reading. Civil rights claimants are victims with no exit (d ≈ 0.95, near-maximum extraction experienced). Lower court judges are constrained victims (d ≈ 0.75, high but not maximal because they retain some discretion in application). The Supreme Court is a beneficiary with arbitrage capacity (d ≈ 0.05, derives institutional benefit but could change the doctrine). Law enforcement is a beneficiary with mobile options (d ≈ 0.15, benefits from protection but could function under statutory regime). Civil rights organizations are mixed: victims in regard to claimants' remedy denial (d ≈ 0.65), beneficiaries in regard to doctrine's focal clarity (d ≈ 0.30). The constitutional accountability framework is an abstract victim (d ≈ 1.0). These directionality values feed the chi calculation: victims with high d and no exit experience maximum effective extraction; beneficiaries with low d experience negative effective extraction (the constraint subsidizes them).
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED: This reading treats qualified immunity as pure snare classification despite the legitimate coordination function that other readings identify (protective_scaffold_reading) or the empirical accountability failure (accountability_void_reading). The resolution strategy for mandatrophy is to deny the legitimacy of the coordination function itself: the doctrine achieves case filtering and operational protection, but it does so through illegitimate means (judicial construction without constitutional/statutory warrant). From the protective_scaffold_reading, the same actions appear as necessary temporary coordination. From the constitutional_fidelity_reading, the same actions appear as illegitimate extraction regardless of their coordinating effect. The mandatrophy is resolved by recognizing that these are irreconcilably different readings of the same kernel — the legitimacy of qualified immunity depends entirely on whether judicial creation without constitutional/statutory text is permissible, and this is a normative question that cannot be resolved by empirical measurement of policy consequences alone.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    constitutional_necessity_thesis,
    'Is some form of official immunity a constitutionally necessary feature of any functioning governance system, or is qualified immunity a judicially fabricated doctrine that could be replaced by statutory immunity schemes without constitutional deficit?',
    'Comparative constitutional law: examination of immunity doctrines in other democracies with robust constitutional protections; historical analysis of pre-qualified_immunity American liability regime; functional analysis of whether alternative statutory immunity regimes (with explicit Congressional authorization) could serve the same coordination functions',
    'If constitutional necessity thesis is true: qualified immunity is defending an illegible constitutional principle (and this reading''s snare classification is incorrect). If false: qualified immunity is pure judicially fabricated extraction with no constitutional warrant, and the snare classification is structurally sound.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constitutional_necessity_thesis, empirical, 'Whether immunity for officials is constitutionally necessary or judicially fabricated').

omega_variable(
    statutory_alternative_feasibility,
    'Could Congress replace judicial qualified immunity with explicit statutory immunity standards without creating worse outcomes for civil rights enforcement or governmental function?',
    'Comparative analysis of statutory immunity regimes (tort law, administrative law, employment law); legislative history of civil rights statutes (42 USC 1983, etc.); analysis of whether judicial construction exceeded Congressional intent; modeling of alternative statutory immunity structures',
    'If statutory alternatives are feasible: the illegitimacy claim is structurable (Congress could replace the doctrine with legitimate authorization). If not feasible: qualified immunity may be a necessary gap-filler despite lacking explicit authorization.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(statutory_alternative_feasibility, empirical, 'Feasibility of statutory immunity replacements for qualified immunity').

omega_variable(
    doctrine_origin_and_scope_creep,
    'Did the Supreme Court intend the original immunity doctrines (from Pierson v. Ray, 1967 and later cases) to develop into the current qualified immunity regime, or has scope creep expanded a narrower immunity into a categorical shield?',
    'Historical textual analysis of Supreme Court opinions; comparison of immunity standards across three eras (pre-1967, 1967-1989 Harlow v. Fitzgerald, post-1989); examination of Supreme Court''s own acknowledgment of doctrinal expansion',
    'If scope creep occurred: the doctrine has become increasingly illegitimate over time (and the extractiveness measurement should show rising trend). If the original cases authorized current scope: the illegitimacy claim rests entirely on whether the original doctrine itself lacked constitutional warrant.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(doctrine_origin_and_scope_creep, empirical, 'Scope creep in qualified immunity doctrine from original intent to current application').

omega_variable(
    committer_framework_ambiguity,
    'Is this constraint characterized by (a) the illegitimacy of judicial construction (reading: the process of judicial doctrine creation without constitutional text or statutory authorization), or (b) the illegitimacy of the substance of qualified immunity as a policy outcome (reading: even if authorized, the doctrine produces unjust results)?',
    'Examination of the reading''s core normative commitment: does it claim illegitimacy derives from lack of constitutional/statutory authorization (framework illegitimacy), or from the doctrine''s effects on constitutional rights (substantive illegitimacy)? This determines whether a statutory immunity scheme with identical substance would satisfy this reading.',
    'If framework illegitimacy: Congressional authorization could legitimize the doctrine (though statutory version might differ). If substantive illegitimacy: even authorized immunity would remain problematic. The reading''s axiom ''judicial_fabrication_illegitimate'' assumes framework illegitimacy; if substantive illegitimacy is the core claim, the reading should be recharacterized.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_framework_ambiguity, conceptual, 'Whether illegitimacy derives from judicial construction process or from substance of qualified immunity').

omega_variable(
    sibling_reading_foreclosure,
    'Does this reading (constitutional fidelity) logically foreclose the protective_scaffold_reading (qualified immunity as necessary temporary structure) or merely coexist with it as competing normative commitments?',
    'Examination of whether the constitutional fidelity reading''s core axiom (judicial construction without constitutional warrant is illegitimate) logically entails that the protective_scaffold_reading''s core axiom (immunity is necessary temporary protection) cannot be true in the same framework. Can both readings be held simultaneously by a single party committed to both constitutional fidelity AND protective policies?',
    'If foreclosure is real: this reading and the scaffold reading are incompatible within any single institutional or normative framework (rare). If they coexist: both are live positions that compete but don''t logically eliminate each other.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure, conceptual, 'Whether constitutional fidelity reading forecloses or coexists with protective scaffold reading').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qualified_immunity_doctrine__constitutional_fidelity_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qimdoc_tr_t0, qualified_immunity_doctrine__constitutional_fidelity_reading, theater_ratio, 0, 0.45).
narrative_ontology:measurement(qimdoc_tr_t15, qualified_immunity_doctrine__constitutional_fidelity_reading, theater_ratio, 15, 0.58).
narrative_ontology:measurement(qimdoc_tr_t30, qualified_immunity_doctrine__constitutional_fidelity_reading, theater_ratio, 30, 0.65).

% Extraction over time
narrative_ontology:measurement(qimdoc_be_t0, qualified_immunity_doctrine__constitutional_fidelity_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(qimdoc_be_t15, qualified_immunity_doctrine__constitutional_fidelity_reading, base_extractiveness, 15, 0.62).
narrative_ontology:measurement(qimdoc_be_t30, qualified_immunity_doctrine__constitutional_fidelity_reading, base_extractiveness, 30, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(qimdoc_su_t0, qualified_immunity_doctrine__constitutional_fidelity_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(qimdoc_su_t15, qualified_immunity_doctrine__constitutional_fidelity_reading, suppression_requirement, 15, 0.68).
narrative_ontology:measurement(qimdoc_su_t30, qualified_immunity_doctrine__constitutional_fidelity_reading, suppression_requirement, 30, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(qualified_immunity_doctrine__constitutional_fidelity_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(qualified_immunity_doctrine__constitutional_fidelity_reading, qualified_immunity_doctrine__protective_scaffold_reading).
narrative_ontology:affects_constraint(qualified_immunity_doctrine__constitutional_fidelity_reading, qualified_immunity_doctrine__accountability_void_reading).
narrative_ontology:affects_constraint(qualified_immunity_doctrine__constitutional_fidelity_reading, qualified_immunity_doctrine__empirical_balancing_reading).
narrative_ontology:affects_constraint(qualified_immunity_doctrine__constitutional_fidelity_reading, section_1983_remedy_denial).
narrative_ontology:affects_constraint(qualified_immunity_doctrine__constitutional_fidelity_reading, civil_rights_enforcement_institutional_capacity).

% DUAL FORMULATION NOTE:
% The qualified_immunity_doctrine kernel has three coexisting readings with structurally distinct constraints. The constitutional_fidelity_reading (this file) denies legitimacy of the doctrine entirely regardless of policy consequences. The protective_scaffold_reading treats the doctrine as necessary temporary protection. The accountability_void_reading treats the doctrine as empirically failed balancing. Each reading produces a separate constraint story with different ε values, different beneficiary/victim declarations, and different classifications from the same structural facts. The network links show which constraints are logically or causally downstream of this reading: section_1983_remedy_denial depends on qualified immunity existing; civil_rights_enforcement_institutional_capacity is affected by the doctrine's suppression of claims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(qualified_immunity_doctrine__constitutional_fidelity_reading, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
