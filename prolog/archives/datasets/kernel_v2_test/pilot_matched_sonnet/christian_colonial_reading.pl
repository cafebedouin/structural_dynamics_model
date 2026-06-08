% ============================================================================
% CONSTRAINT STORY: christian_colonial_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-08
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_christian_colonial_reading, []).

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
 *   constraint_id: christian_colonial_reading
 *   human_readable: Christian Colonial Marriage Authority Reading
 *   domain: comparative_law/legal_pluralism/constitutional_theory
 *
 * SUMMARY:
 *   The Christian marriage law regime in India represents a colonial
 *   codification of ecclesiastical tradition that persists as binding statute
 *   enforced by secular courts. The Indian Christian Marriage Act (1872) and
 *   Indian Divorce Act (1869, amended 2001) froze Anglican ecclesiastical
 *   doctrine at a specific historical moment and transplanted it into a
 *   post-independence constitutional framework that guarantees religious
 *   freedom and personal law autonomy. This reading of the marriage authority
 *   kernel holds that legitimate marriage authority derives from
 *   ecclesiastical tradition as mediated through colonial statute, with
 *   secular courts as enforcement agents. The 2001 amendment (introducing
 *   no-fault divorce after 2 years separation) represents the first major
 *   substantive reform in 130 years, demonstrating that the structure is
 *   mutable through legislative process but does not self-correct. The
 *   constraint exhibits tangled-rope structure: genuine coordination function
 *   (legal recognition, succession rules, dispute resolution) layered with
 *   extraction (gender asymmetry in pre-2001 fault grounds, exit barriers,
 *   procedural burden on interfaith couples under Special Marriage Act).
 *   Theater ratio (0.58) reflects that courts apply 'ecclesiastical
 *   tradition' through statutory interpretation, but the tradition's
 *   authority post-independence is partly performative — the legitimacy claim
 *   (British colonial codification of Christian doctrine) is hollow, yet the
 *   form persists through institutional inertia and community veto power.
 *
 * KEY AGENTS:
 *   - Women Seeking Divorce Pre-2001: Primary victim (powerless/trapped) — no-fault divorce prohibition created absolute barrier to exit from failed marriages until 2001 amendment
 *   - Interfaith Couples: Secondary victim (moderate/constrained) — higher procedural burden under Special Marriage Act, social penalty, loss of community standing; but system enables interfaith marriage (mixed coordination-extraction)
 *   - Christian Community Institutions: Primary beneficiary (institutional/arbitrage) — ecclesiastical authority codified into state law preserves institutional gatekeeping; can advocate for amendments when doctrine becomes untenable
 *   - Secular Court System: Institutional actor (institutional/constrained) — coordinates enforcement of plural personal law codes but constrained by having to enforce archaic ecclesiastical doctrine as binding statute
 *   - Law Reform Advocates: Organized agents (organized/mobile) — see colonial codification as temporary; 2001 amendment demonstrates legislative reform pathway
 *   - Colonial Statute Framework: Institutional form (institutional/arbitrage) — persists through inertia; legitimacy claim is hollow post-independence (piton perspective)
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees hybrid coordination-extraction structure with path dependence from colonial codification
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(christian_colonial_reading, 0.48).
domain_priors:suppression_score(christian_colonial_reading, 0.62).
domain_priors:theater_ratio(christian_colonial_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(christian_colonial_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(christian_colonial_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(christian_colonial_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(christian_colonial_reading, accessibility_collapse, 0.15).
narrative_ontology:constraint_metric(christian_colonial_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(christian_colonial_reading, tangled_rope).
narrative_ontology:human_readable(christian_colonial_reading, "Christian Colonial Marriage Authority Reading").
narrative_ontology:topic_domain(christian_colonial_reading, "comparative_law/legal_pluralism/constitutional_theory").

domain_priors:requires_active_enforcement(christian_colonial_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(christian_colonial_reading, '3ae687df-8e7b-43bb-8d1b-46dc7169c685').
narrative_ontology:cs_kernel_codification('3ae687df-8e7b-43bb-8d1b-46dc7169c685', formalized).
narrative_ontology:cs_authority_grounding('3ae687df-8e7b-43bb-8d1b-46dc7169c685', lineage).
narrative_ontology:cs_interpretation_layer_present('3ae687df-8e7b-43bb-8d1b-46dc7169c685').
narrative_ontology:cs_reading_relation('3ae687df-8e7b-43bb-8d1b-46dc7169c685', christian_colonial_reading__hindu_codified_reading, coexists_with).
narrative_ontology:cs_reading_relation('3ae687df-8e7b-43bb-8d1b-46dc7169c685', christian_colonial_reading__muslim_shariat_reading, coexists_with).
narrative_ontology:cs_reading_relation('3ae687df-8e7b-43bb-8d1b-46dc7169c685', christian_colonial_reading__parsi_community_reading, coexists_with).
narrative_ontology:cs_reading_relation('3ae687df-8e7b-43bb-8d1b-46dc7169c685', christian_colonial_reading__secular_contractual_reading, influences).
narrative_ontology:cs_axiom('3ae687df-8e7b-43bb-8d1b-46dc7169c685', foundational, ecclesiastical_permanence_primacy).
narrative_ontology:cs_axiom_status(ecclesiastical_permanence_primacy, overridden).
narrative_ontology:cs_axiom_grounding('3ae687df-8e7b-43bb-8d1b-46dc7169c685', ecclesiastical_permanence_primacy, deontological).
narrative_ontology:cs_axiom('3ae687df-8e7b-43bb-8d1b-46dc7169c685', secondary, colonial_codification_legitimacy).
narrative_ontology:cs_axiom_status(colonial_codification_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('3ae687df-8e7b-43bb-8d1b-46dc7169c685', colonial_codification_legitimacy, conventional).
narrative_ontology:cs_reference_frame('3ae687df-8e7b-43bb-8d1b-46dc7169c685', anglican_ecclesiastical_permanence_doctrine).
narrative_ontology:cs_drift_state('3ae687df-8e7b-43bb-8d1b-46dc7169c685', post_2001_amendment, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('3ae687df-8e7b-43bb-8d1b-46dc7169c685', '').
narrative_ontology:cs_kernel_id(christian_colonial_reading, marriage_authority_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(christian_colonial_reading, christian_community_institutions).
narrative_ontology:constraint_beneficiary(christian_colonial_reading, secular_court_system).
narrative_ontology:constraint_beneficiary(christian_colonial_reading, legal_profession_matrimonial_bar).
narrative_ontology:constraint_victim(christian_colonial_reading, women_seeking_divorce_pre_2001).
narrative_ontology:constraint_victim(christian_colonial_reading, interfaith_couples).
narrative_ontology:constraint_victim(christian_colonial_reading, non_christian_minorities_under_special_marriage_act).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: WOMEN SEEKING DIVORCE PRE-2001 (SNARE) — Trapped by no-fault divorce prohibition until 2001 amendment. Ecclesiastical tradition codified as statute created absolute barrier to exit from failed marriages. Maximum extraction: legal identity locked into marital status with no unilateral exit path. Secular courts enforced ecclesiastical permanence doctrine.
constraint_indexing:constraint_classification(christian_colonial_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: INTERFAITH COUPLES (TANGLED ROPE) — Constrained by requirement to navigate Special Marriage Act (secular alternative) vs community-specific codes. Genuine coordination function: legal recognition of marriage across religious boundaries. But extraction: higher procedural burden, social penalty, loss of community standing. Mixed experience: the system both enables interfaith marriage (coordination) and penalizes it (extraction).
constraint_indexing:constraint_classification(christian_colonial_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CHRISTIAN COMMUNITY INSTITUTIONS (ROPE) — Primary beneficiaries. Ecclesiastical authority codified into state law preserves institutional gatekeeping over marriage validity, divorce grounds, and succession. Arbitrage exit: institutions can advocate for amendments (2001 divorce reform) when doctrine becomes untenable. Net beneficiary: extraction flows toward institutional authority, not away from it.
constraint_indexing:constraint_classification(christian_colonial_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: SECULAR COURT SYSTEM (TANGLED ROPE) — Courts coordinate enforcement of plural personal law codes (genuine function) but are constrained by having to enforce archaic ecclesiastical doctrine as binding statute. Judges cannot reform substantive law, only interpret it. Mixed position: benefits from matrimonial jurisdiction (caseload, institutional role) but constrained by having to apply doctrine they may view as unjust.
constraint_indexing:constraint_classification(christian_colonial_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: LAW REFORM ADVOCATES (SCAFFOLD) — Organized civil society groups see the colonial codification as temporary: the 2001 divorce amendment demonstrates that ecclesiastical doctrine codified as statute CAN be reformed through legislative process. Sunset logic: as constitutional equality norms mature and women's rights advocacy strengthens, the archaic elements will be progressively amended. Mobile exit: advocates can shift to constitutional litigation if legislative reform stalls.
constraint_indexing:constraint_classification(christian_colonial_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: COLONIAL STATUTE FRAMEWORK (PITON) — The form persists (ecclesiastical tradition as binding statute) but the function has atrophied. Post-independence, the legitimacy claim (British colonial authority codifying Christian doctrine) is hollow — the statute remains because institutional inertia and community veto power prevent wholesale replacement, not because the colonial framing retains normative force. Theater: courts apply 'ecclesiastical tradition' through statutory interpretation, but the tradition's authority is performative.
constraint_indexing:constraint_classification(christian_colonial_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From a comparative law perspective, this reading instantiates a hybrid: genuine coordination function (legal recognition of Christian marriages, succession rules, dispute resolution) layered with extraction (gender asymmetry, exit barriers, minority penalty under Special Marriage Act). The colonial codification froze ecclesiastical doctrine at a specific historical moment, creating path dependence. The 2001 amendment shows the structure is mutable but requires legislative action — it does not self-correct.
constraint_indexing:constraint_classification(christian_colonial_reading, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(christian_colonial_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(christian_colonial_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(christian_colonial_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(christian_colonial_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(christian_colonial_reading, TR),
    TR >= 0.70.

:- end_tests(christian_colonial_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48): Moderate. The 2001 amendment reduced extraction significantly (from 0.58 pre-amendment) by removing the absolute barrier to divorce, but residual extraction persists: fault-based grounds remain gendered (adultery by wife is per se ground; adultery by husband requires aggravating circumstance), procedural costs are high, and interfaith couples face penalty under Special Marriage Act. The value reflects that much of the extraction was removed by reform, but the structure retains asymmetric costs. Suppression (0.62): Moderate-high. Significant barriers remain: social penalty for divorce within Christian community, economic dependency (especially for women in traditional marriages), legal costs of contested proceedings, and lack of alternative dispute resolution mechanisms. The 2001 amendment reduced suppression (from 0.68) by providing exit path, but exit remains costly. Theater ratio (0.58): Moderate-high. Courts apply 'ecclesiastical tradition' through statutory interpretation, but post-independence the tradition's authority is partly performative. The legitimacy claim (colonial codification of Christian doctrine) is hollow — the statute remains because institutional inertia and community veto prevent replacement, not because the colonial framing retains normative force. Theater has increased over the interval (from 0.35 at colonial era) as the gap between the legitimacy claim and the actual authority source has widened.
 *
 * PERSPECTIVAL GAP:
 *   The Christian community institutions see coordination (Rope) — the statute preserves their authority and enables them to advocate for amendments when needed. Women seeking divorce pre-2001 saw pure extraction (Snare) — absolute barrier to exit with no alternative. Law reform advocates see a temporary problem with a sunset (Scaffold) — the 2001 amendment demonstrates that legislative reform is possible, and further liberalization is expected as constitutional equality norms mature. The colonial statute framework sees its own degraded ritual (Piton) — the form persists but the legitimacy claim is hollow. Interfaith couples and the secular court system see mixed coordination-extraction (Tangled Rope) — the system both enables and constrains. The analytical observer sees the hybrid structure and recognizes that the 2001 amendment represents a critical juncture: if further reforms follow, the scaffold perspective is validated; if 2001 stands alone, the constraint has stabilized at a new equilibrium with residual extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Christian community institutions are primary beneficiaries: ecclesiastical authority codified as statute preserves institutional gatekeeping over marriage validity, divorce grounds, and succession. The secular court system is in a mixed position: benefits from matrimonial jurisdiction (caseload, institutional role) but constrained by having to enforce doctrine judges may view as unjust. Women seeking divorce pre-2001 were primary victims: trapped by no-fault prohibition with no unilateral exit. Interfaith couples are secondary victims: the system enables interfaith marriage (coordination function) but penalizes it through higher procedural burden and social costs (extraction function). Law reform advocates have mobile exit: can shift to constitutional litigation if legislative reform stalls. The analytical observer sees the full hybrid structure: genuine coordination layered with extraction, mutable through legislative process but not self-correcting.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (legal recognition and regulation of Christian marriages) has NOT outlived its function — the coordination role remains live. But the FORM of the mandate (ecclesiastical tradition codified by colonial statute) is partly mandatrophic: the colonial legitimacy claim is hollow post-independence, yet the structure persists. The 2001 amendment demonstrates partial mandatrophy resolution: the most egregious archaic element (no-fault divorce prohibition) was removed when it became untenable, but the broader structure (ecclesiastical authority as statutory source) remains. The piton perspective captures this: the tradition's authority is performative, maintained through institutional inertia rather than normative force. Full mandatrophy resolution would require either (a) wholesale replacement with secular contractual framework (the secular_contractual_reading sibling), or (b) devolution of authority back to ecclesiastical bodies with state recognition but not enforcement (disestablishment model). Neither has occurred, so the constraint remains in a hybrid state: partly functional, partly theatrical.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Is this constraint a reading of the marriage authority kernel, or is the kernel itself (the contested question of which authority grounds marriage legitimacy) the constraint?',
    'Committer-frame analysis: if different parties hold different readings of the SAME persisting commitment (the Constitution''s recognition of personal law), then each reading is a distinct constraint. If the parties are disputing what the commitment IS, then the kernel itself is the constraint.',
    'If readings: this story is one of five sibling constraints (christian_colonial, hindu_codified, muslim_shariat, parsi_community, secular_contractual). If kernel: this story should be reframed as one perspective within a single constraint story about constitutional pluralism.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Whether this is a kernel reading or a perspective within a kernel constraint').

omega_variable(
    ecclesiastical_authority_grounding,
    'Does ecclesiastical authority derive from divine mandate (theological grounding) or from community consent (conventional grounding)?',
    'Doctrinal analysis of Christian marriage theology vs sociological analysis of how authority is actually maintained. If authority persists only because the community accepts it, grounding is conventional. If authority is claimed regardless of community acceptance, grounding is theological.',
    'If theological: the reading''s axioms are deontological and cannot be empirically overridden. If conventional: the reading''s axioms are subject to community revision, and the 2001 amendment represents axiom drift rather than axiom override.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ecclesiastical_authority_grounding, conceptual, 'Whether ecclesiastical authority is theological or conventional').

omega_variable(
    secular_court_capture,
    'Are secular courts enforcing ecclesiastical doctrine because they are institutionally captured by Christian community lobbying, or because constitutional pluralism requires them to enforce community-specific personal law?',
    'Comparative analysis: do courts enforce other communities'' personal law codes with equal deference, or is Christian law privileged? Historical analysis: did Christian community institutions lobby for statutory codification, or was codification a colonial administrative decision?',
    'If capture: the constraint is more extractive than the base metrics suggest (institutional beneficiaries are actively suppressing alternatives). If constitutional pluralism: the constraint is more coordinative (courts are neutrally enforcing a pluralist framework).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(secular_court_capture, empirical, 'Whether secular court enforcement reflects capture or neutral pluralism').

omega_variable(
    amendment_trajectory,
    'Does the 2001 divorce amendment represent a one-time correction of the most egregious archaic element, or the beginning of a systematic liberalization trajectory?',
    'Longitudinal analysis of subsequent amendments and judicial interpretation. If further reforms follow (e.g., gender-equal succession, removal of fault-based divorce grounds), trajectory is systematic. If 2001 stands alone for 20+ years, it was a one-time correction.',
    'If systematic trajectory: scaffold perspective is validated (sunset is real). If one-time correction: the constraint has stabilized at a new equilibrium with residual extraction, and scaffold perspective is aspirational.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(amendment_trajectory, empirical, 'Whether 2001 amendment begins a liberalization trajectory or stands alone').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(christian_colonial_reading, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(chr_col_theater_colonial_era, christian_colonial_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(chr_col_theater_independence, christian_colonial_reading, theater_ratio, 25, 0.45).
narrative_ontology:measurement(chr_col_theater_pre_amendment, christian_colonial_reading, theater_ratio, 50, 0.55).
narrative_ontology:measurement(chr_col_theater_post_2001, christian_colonial_reading, theater_ratio, 55, 0.58).
narrative_ontology:measurement(chr_col_theater_contemporary, christian_colonial_reading, theater_ratio, 75, 0.58).

% Extraction over time
narrative_ontology:measurement(chr_col_extract_colonial_era, christian_colonial_reading, base_extractiveness, 0, 0.65).
narrative_ontology:measurement(chr_col_extract_independence, christian_colonial_reading, base_extractiveness, 25, 0.62).
narrative_ontology:measurement(chr_col_extract_pre_amendment, christian_colonial_reading, base_extractiveness, 50, 0.58).
narrative_ontology:measurement(chr_col_extract_post_2001, christian_colonial_reading, base_extractiveness, 55, 0.48).
narrative_ontology:measurement(chr_col_extract_contemporary, christian_colonial_reading, base_extractiveness, 75, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(chr_col_suppress_colonial_era, christian_colonial_reading, suppression_requirement, 0, 0.75).
narrative_ontology:measurement(chr_col_suppress_independence, christian_colonial_reading, suppression_requirement, 25, 0.72).
narrative_ontology:measurement(chr_col_suppress_pre_amendment, christian_colonial_reading, suppression_requirement, 50, 0.68).
narrative_ontology:measurement(chr_col_suppress_post_2001, christian_colonial_reading, suppression_requirement, 55, 0.62).
narrative_ontology:measurement(chr_col_suppress_contemporary, christian_colonial_reading, suppression_requirement, 75, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(christian_colonial_reading, identity_coordination).
narrative_ontology:affects_constraint(christian_colonial_reading, hindu_codified_reading).
narrative_ontology:affects_constraint(christian_colonial_reading, muslim_shariat_reading).
narrative_ontology:affects_constraint(christian_colonial_reading, parsi_community_reading).
narrative_ontology:affects_constraint(christian_colonial_reading, secular_contractual_reading).

% DUAL FORMULATION NOTE:
% The marriage_authority_kernel decomposes into five sibling readings, each with distinct ε values reflecting different degrees of extraction. The christian_colonial reading has moderate extraction (0.48 post-2001) due to residual gender asymmetry and exit barriers. The hindu_codified reading likely has lower extraction due to earlier and more comprehensive reform (1955 Act with multiple amendments). The muslim_shariat reading likely has higher extraction due to unilateral talaq and minimal codification. The parsi_community reading likely has lowest extraction due to liberal divorce provisions from inception (1936). The secular_contractual reading likely has low extraction but high procedural burden (30-day notice period, magistrate approval). Each reading is a distinct constraint; the kernel is the contested commitment they all claim to instantiate.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(christian_colonial_reading, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
