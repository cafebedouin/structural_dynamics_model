% ============================================================================
% CONSTRAINT STORY: hindu_codified_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hindu_codified_reading, []).

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
 *   constraint_id: hindu_codified_reading
 *   human_readable: Hindu Codified Marriage Authority: Dharmashastra Principles Under Constitutional Oversight
 *   domain: comparative_law/legal_pluralism/constitutional_theory
 *
 * SUMMARY:
 *   This constraint models how marriage authority in India derives from
 *   codified dharmashastra principles enforced through secular constitutional
 *   law and parliamentary amendment. Since independence (1947) and the
 *   adoption of the Constitution (1950), Hindu marriage has been governed by
 *   a hybrid system: the Hindu Marriage Act (1956) and Hindu Succession Act
 *   (1956) codified selected dharmashastra principles regarding marriage,
 *   maintenance, succession, and divorce. This codification was presented as
 *   a recovery and rationalization of 'essential' Hindu law principles, but
 *   in operation it created a statutory regime subject to parliamentary
 *   amendment and constitutional judicial review. The constraint exhibits a
 *   deep reading battle: whether marriage authority derives from traditional
 *   dharmashastra interpretation (continuous religious tradition) or from
 *   secular statutory law that merely references dharmashastra origins
 *   (secular codification with religious genealogy). The extractiveness
 *   trajectory (0.28→0.38) shows modest accumulation as amendments expanded
 *   women's inheritance and divorce rights while preserving male-preferential
 *   succession rules and patrilineal framing. Suppression (0.55→0.42) has
 *   declined as statutory rights expanded, though identity-locked exit
 *   mechanisms and interfaith marriage restrictions persist. Theater
 *   (0.25→0.35) has risen as the constraint becomes increasingly
 *   self-consciously performed: courts adjudicate what counts as 'essential
 *   dharma' (performative gatekeeping), and the codification persists through
 *   institutional inertia rather than continuous traditioning. The analytical
 *   observer at civilizational scope risks reading this as a mountain
 *   (structural necessity of religious pluralism), but the structural data
 *   reveals it as a false summit: a contingent institutional choice to
 *   privilege codification of majority-tradition marriage law over equal
 *   civil alternatives.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hindu_codified_reading, 0.38).
domain_priors:suppression_score(hindu_codified_reading, 0.42).
domain_priors:theater_ratio(hindu_codified_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hindu_codified_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(hindu_codified_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(hindu_codified_reading, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hindu_codified_reading, tangled_rope).
narrative_ontology:human_readable(hindu_codified_reading, "Hindu Codified Marriage Authority: Dharmashastra Principles Under Constitutional Oversight").
narrative_ontology:topic_domain(hindu_codified_reading, "comparative_law/legal_pluralism/constitutional_theory").

domain_priors:requires_active_enforcement(hindu_codified_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hindu_codified_reading, 'b6ba2750-d9e4-4337-affd-6b9a463e4518').
narrative_ontology:cs_kernel_codification('b6ba2750-d9e4-4337-affd-6b9a463e4518', formalized).
narrative_ontology:cs_authority_grounding('b6ba2750-d9e4-4337-affd-6b9a463e4518', lineage).
narrative_ontology:cs_interpretation_layer_present('b6ba2750-d9e4-4337-affd-6b9a463e4518').
narrative_ontology:cs_reading_relation('b6ba2750-d9e4-4337-affd-6b9a463e4518', hindu_codified_reading__muslim_shariat_reading, coexists_with).
narrative_ontology:cs_reading_relation('b6ba2750-d9e4-4337-affd-6b9a463e4518', hindu_codified_reading__secular_contractual_reading, coexists_with).
narrative_ontology:cs_reading_relation('b6ba2750-d9e4-4337-affd-6b9a463e4518', hindu_codified_reading__christian_colonial_reading, coexists_with).
narrative_ontology:cs_reading_relation('b6ba2750-d9e4-4337-affd-6b9a463e4518', hindu_codified_reading__parsi_community_reading, coexists_with).
narrative_ontology:cs_axiom('b6ba2750-d9e4-4337-affd-6b9a463e4518', foundational, dharmashastra_continuous_religious_tradition).
narrative_ontology:cs_axiom_status(dharmashastra_continuous_religious_tradition, holdable).
narrative_ontology:cs_axiom_grounding('b6ba2750-d9e4-4337-affd-6b9a463e4518', dharmashastra_continuous_religious_tradition, deontological).
narrative_ontology:cs_axiom('b6ba2750-d9e4-4337-affd-6b9a463e4518', foundational, statutory_codification_preserves_essential_character).
narrative_ontology:cs_axiom_status(statutory_codification_preserves_essential_character, holdable).
narrative_ontology:cs_axiom_grounding('b6ba2750-d9e4-4337-affd-6b9a463e4518', statutory_codification_preserves_essential_character, instrumental).
narrative_ontology:cs_reference_frame('b6ba2750-d9e4-4337-affd-6b9a463e4518', dharmashastra_principles_recovered_and_codified).
narrative_ontology:cs_drift_state('b6ba2750-d9e4-4337-affd-6b9a463e4518', contemporary_2026, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('b6ba2750-d9e4-4337-affd-6b9a463e4518', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(hindu_codified_reading, marriage_authority_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hindu_codified_reading, dharmashastra_jurisprudence).
narrative_ontology:constraint_beneficiary(hindu_codified_reading, male_spousal_authority).
narrative_ontology:constraint_beneficiary(hindu_codified_reading, patrilineal_inheritance_claims).
narrative_ontology:constraint_victim(hindu_codified_reading, women_spousal_exit).
narrative_ontology:constraint_victim(hindu_codified_reading, non_orthodox_family_arrangements).
narrative_ontology:constraint_victim(hindu_codified_reading, interfaith_marriage_access).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: WIFE UNDER CODIFIED DHARMASHASTRA (SNARE) — Structurally mobile (can divorce, remarry under statute) but identity-locked: self-concept constituted through dharmic duty (pativrata, wife-as-protector-of-household). Exit is structurally possible but requires abandoning the identity frame that makes the marriage relationship intelligible from within. Statutory grounds for divorce (1976: cruelty, desertion, adultery) are formally available but socially costly. Experiences maximum extraction through the identity frame that prevents its exercise.
constraint_indexing:constraint_classification(hindu_codified_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(national))).

% PERSPECTIVE 2: DHARMASHASTRA JURISPRUDENCE / CODIFICATION AUTHORITY (ROPE) — Institutional beneficiary (judges, legal scholars, marriage registrars). Benefits from the constraint's coordination function: codification provides stable rules for marriage registration, succession, maintenance, and divorce. The 1976 amendment (expanding divorce grounds) was itself a coordinating act. Arbitrage exit (can revise the code through amendment). Experiences the constraint as coordination — it solves the genuine problem of creating a unifying legal framework across regional and variant dharmashastra traditions. Theater low — the codification is genuinely functional, not performative.
constraint_indexing:constraint_classification(hindu_codified_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: HUSBAND AND PATRILINEAL HEIR (TANGLED ROPE) — Mixed position. Coordinates with the wife on household economy, child-rearing, property management. But also benefits from asymmetric extraction: succession law privileges male heirs; maintenance obligations are asymmetric (husband responsible for wife, not vice versa, even in modern amendments); divorce grounds differ by gender (e.g., adultery historically required female chastity but not male fidelity). Exit is constrained by property entanglement, social status in extended family, and inheritance expectations. Genuine coordination function paired with asymmetric extraction.
constraint_indexing:constraint_classification(hindu_codified_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: CONSTITUTIONAL COURTS AND PARLIAMENTARY OVERRIDE (TANGLED ROPE) — Powerful institutional actors with formal amendment power. Coordinates the marriage domain with constitutional principles (Articles 14, 15: equality, non-discrimination). But also extracts legitimacy: courts claim authority to adjudicate which dharmashastra principles are 'essential' (can be constitutionally protected) vs 'mutable' (can be reformed). 1976 amendment (Hindu Succession Act expansion of female inheritance) and 1956 amendment (legitimacy of interfaith marriage) show active reforming power. Experiences the constraint as coordination problem with extraction: can amend, but amendments are incremental and must preserve 'essential' religious character, limiting exit.
constraint_indexing:constraint_classification(hindu_codified_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: HINDU PERSONAL LAW ESTABLISHMENT (PITON) — Judges, law professors, bar councils, succession registrars. The institutional apparatus maintains the distinction between 'essential religious character' (dharmashastra principles preserved) and 'secular administration' (courts apply the law). Theater is substantial: the constraint persists as a category despite centuries of amendment and reinterpretation. The claim that codified rules derive from dharmashastra principles is increasingly performative — the rules are now secular statutory law with religious genealogy, not living dharmic interpretation. Suppression of reform is moderate (amendments happen) but constraint persists through institutional inertia. Organized actors are constrained because the establishment's legitimacy depends on maintaining the dharmashastra framing, even as the substance diverges.
constraint_indexing:constraint_classification(hindu_codified_reading, piton,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / RELIGIOUS PLURALISM VIEW (MOUNTAIN) — From a civilizational analytical perspective, the constraint appears as a structural necessity: multi-religious polities must have SOME mechanism for recognizing distinct marriage regimes. Codification of dharmashastra alongside Muslim shariat, Christian canon, Parsi custom, and civil law appears as an immutable feature of constitutional pluralism. But the structural data contradicts this: the constraint privileges certain traditions (Hindu as majority, benefiting from codification advantages) while marginalizing others (interfaith marriage, non-orthodox arrangements). The engine will compute this as a false summit — naturalizing what is actually a contingent institutional choice (privileging tradition-based codification for majority religions over equal civil alternatives).
constraint_indexing:constraint_classification(hindu_codified_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hindu_codified_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(hindu_codified_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(hindu_codified_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(hindu_codified_reading, TR),
    TR >= 0.70.

:- end_tests(hindu_codified_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   EXTRACTIVENESS (0.38): Moderate. The constraint exhibits genuine coordination function (marriage registration, maintenance obligations, succession clarity) alongside asymmetric extraction (male-preferential succession, husband's legal authority over wife's domicile and earnings, gender-differentiated adultery grounds historically). The 1976 amendment expanded female inheritance rights and 1986 amendments expanded maintenance rights, reducing extraction modestly. But core asymmetries persist: succession still favors sons in traditional Hindu law framing, and identity-locked exit mechanisms prevent women from exercising formal divorce rights despite statutory availability. The 0.38 value reflects that extraction is real but tempered by statutory equality protections that courts increasingly enforce. SUPPRESSION (0.42): Moderate. Structural barriers (legal restrictions on interfaith marriage, registration requirements favoring joint household) are moderate; internalized barriers (identity-locked exit, family and community pressure) are substantial. The 0.42 value reflects declining formal suppression but persistent informal suppression. THEATER (0.35): Moderate-low. The codification is functionally operational (courts apply the rules, they solve real problems), but theater is rising as courts perform the gatekeeping function of adjudicating 'essential' dharmashastra principles. The increasing theater reflects that amendments have created a gap between codified dharmashastra framing and actual equal-protection jurisprudence — courts maintain the dharmashastra label while reforming the substance, creating performative continuity.
 *
 * PERSPECTIVAL GAP:
 *   The wife (powerless/identity_locked) sees snare: formally available divorce grounds are practically inaccessible due to identity-fusion with dharmic duty; exit requires becoming a 'different woman' with no social role. The dharmashastra codification authority (institutional/arbitrage) sees rope: codification solved the real problem of unifying regional dharmashastra variants and creating stability for marriage registration and succession. The husband (moderate/constrained) sees tangled rope: genuine coordination on household economy and childcare, but also benefits from asymmetric succession and legal authority. The constitutional courts (powerful/arbitrage) see tangled rope: they coordinate between dharmashastra traditions and constitutional equality, but extract legitimacy through gatekeeping what counts as 'essential' — they can override dharmashastra in the name of equality, but only when courts decide. The personal law establishment (organized/constrained) sees piton: the institutional apparatus maintains the dharmashastra/secular distinction despite centuries of amendment, preserving a category increasingly divorced from its content. The analytical observer risks seeing mountain: religious pluralism requires SOME marriage regime recognition, making codification structurally necessary — but this naturalizes what is actually a choice to privilege majority-tradition codification over equal civil alternatives.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each agent derives from their structural relationship to extraction flow and exit options. The wife (d ≈ 0.85): trapped by identity-lock (cannot exercise exit even when structurally mobile), victim of extraction (bears costs of patrilineal succession, legally prescribed authority), powerless to reform. The dharmashastra authority (d ≈ 0.15): beneficiary (authority derives from codification, benefits from institutional role as interpreter), arbitrage exit (can revise via amendment). The husband (d ≈ 0.60): beneficiary in succession asymmetry, but also constrained by maintenance obligations and emerging equal-protection doctrines; moderate power gives partial exit (can negotiate, less bound by tradition than wife). The courts (d ≈ 0.40): partial beneficiaries (extract legitimacy through gatekeeping 'essential' dharma), but also constrained by constitutional mandate to protect equality; powerful exit (can reinterpret or override). The personal law establishment (d ≈ 0.35): beneficiary in maintaining institutional category, but constrained by need to match reformed substance with traditional framing; organized power but limited exit. The analytical observer (d = 0.5): perfectly symmetric position; sees extraction and coordination symmetrically; no net directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The constraint's founding problem (unifying marriage law across regional and religious variation in a newly independent state) remains partially live. The 1956 codification solved the acute problem of creating a uniform legal framework for Hindu marriage across colonial-era fragmentation. But a second problem has emerged: the tension between dharmashastra codification (which privileges continuous tradition) and constitutional equality (which requires gender-neutral rules). Amendments have addressed the equality problem incrementally (1976 succession, 1986 maintenance), but each amendment creates performative work: courts must adjudicate what is 'essential' dharmashastra (protected) vs mutable statutory content (subject to reform). The theater ratio has risen (0.25→0.35) because courts increasingly perform gatekeeping rather than adjudicate. The constraint exhibits PITON characteristics (theatrical maintenance of a distinction between dharmashastra codification and secular administration) alongside TANGLED ROPE characteristics (genuine coordination paired with extraction). Mandatrophy is not resolved because the underlying reading battle is unresolved: is marriage authority derived from continuous dharmashastra tradition (making the codification a recovery), or from secular statutory law that merely references dharmashastra (making the codification a retrospective framing)? The amendment trajectory suggests secular equality is gaining ground, but the theater of dharmashastra framing persists.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    essential_vs_mutable_dharma_boundary,
    'What makes a dharmashastra principle ''essential'' (constitutionally protected) versus ''mutable'' (subject to amendment)? Is the boundary drawn by courts or by tradition?',
    'Historical analysis of court decisions on which principles were deemed essential (e.g., patrilineal succession) vs mutable (divorce grounds, female inheritance). Examination of whether courts applied consistent criteria or treated the boundary as contestable.',
    'If boundary is stable and tradition-derived: mountain classification gains credibility (immutable features of dharmic law). If boundary is court-determined and shifts with political majorities: tangled_rope classification confirmed (courts extract legitimacy through gatekeeping what counts as essential). If boundary collapses: snare classification for women and non-orthodox parties (no protection against amendment that erodes their interests).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(essential_vs_mutable_dharma_boundary, conceptual, 'The boundary between essential dharmashastra principles and mutable statutory content').

omega_variable(
    identity_lock_persistence_post_exit,
    'Do women who exit codified marriages (through divorce or desertion of dharmic identity) report lasting effects of the identity-lock mechanism, or does the lock dissolve once structural barriers are removed?',
    'Longitudinal interviews with divorced/separated women; measurement of whether women retain internalized dharmic marriage duty even after legal exit; comparison with women from secular legal regimes (no dharmashastra frame) to isolate identity-lock effects.',
    'If identity-lock persists post-exit: the suppression value (0.42) is understated; suppression includes internalized barriers that exit does not remove. Reclassify as higher suppression (0.55+) and snare from the wife''s perspective becomes more secure. If identity-lock dissolves upon structural exit: suppression is accurate; the constraint is what authors it (codified rules), not what internals persist.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_persistence_post_exit, empirical, 'Whether identity-lock mechanisms persist after structural exit from codified marriage').

omega_variable(
    dharmashastra_reading_vs_secular_contractual_reading,
    'Does codification of dharmashastra principles foreclose, coexist with, or influence the secular-contractual reading of marriage (marriage as civil contract between equals)?',
    'Constitutional jurisprudence analysis: do courts treat dharmashastra codification and secular marriage contract as logically incompatible (foreclose) or as coexisting alternative frameworks (coexist)? Evidence: cases where interfaith or same-sex couples claim marriage rights under secular contract frame vs courts'' application of dharmashastra-derived restrictions.',
    'If foreclosed: the two readings cannot occupy the same constitutional framework; courts must choose. If coexist: both frames are live; marriage is simultaneously a dharmic bond and a civil contract. If influences: dharmashastra codification creates structural pressure on secular marriage (e.g., by requiring ''essential religious character'' even in secular marriages, or by privileging majority-religion frameworks).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(dharmashastra_reading_vs_secular_contractual_reading, conceptual, 'Logical and institutional relationship between dharmashastra and secular-contractual readings').

omega_variable(
    amendment_asymmetry_direction,
    'Over the 1956-present interval, are amendments to codified Hindu marriage law trending toward dharmashastra traditionalism or toward secular equality? Is the trend symmetric or is there directional asymmetry?',
    'Timeline of major amendments (1956 Hindu Marriage Act, 1976 Hindu Succession Act, 1986 maintenance amendments, 2005 Hindu Succession Act amendment). Classification by direction: traditionalism, equality, or neutral. Statistical analysis of whether amendments cluster in one direction or oscillate.',
    'If trending toward equality: the secular-contractual reading is gaining institutional ground; dharmashastra authority is eroding. If trending toward traditionalism: dharmashastra codification is reasserting. If oscillating: the constraint is contested and the underlying reading battle is unresolved — interpretation_layer_present=true captures the oscillation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(amendment_asymmetry_direction, empirical, 'Directional trend in Hindu marriage law amendments (traditionalism vs equality)').

omega_variable(
    interfaith_marriage_suppression_mechanism,
    'Is suppression of interfaith marriage in codified Hindu law (Section 5 restrictions) structural (legal prohibition), internalized (family and community pressure), or both? What is the proportion?',
    'Comparative data: legal barriers to interfaith marriage vs actual interfaith marriage rates vs reported family/community obstruction. Analysis of whether removing legal barriers (as some states have done) decreases suppression or merely formalizes existing internalized barriers.',
    'If legal barriers are primary: suppression value (0.42) is accurate and removal would lower extraction significantly. If internalized barriers are primary: suppression persists despite legal reform, and removing codified restrictions has modest effect. If both: suppression has a durable structural plus internalized component; partial removal leaves significant suppression in place.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interfaith_marriage_suppression_mechanism, empirical, 'Proportion of structural vs internalized suppression of interfaith marriage').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hindu_codified_reading, 0, 49).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hcr_theater_1956, hindu_codified_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(hcr_theater_1976, hindu_codified_reading, theater_ratio, 20, 0.33).
narrative_ontology:measurement(hcr_theater_2005, hindu_codified_reading, theater_ratio, 49, 0.35).

% Extraction over time
narrative_ontology:measurement(hcr_base_extract_1956, hindu_codified_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(hcr_base_extract_1966, hindu_codified_reading, base_extractiveness, 10, 0.33).
narrative_ontology:measurement(hcr_base_extract_1976, hindu_codified_reading, base_extractiveness, 20, 0.38).
narrative_ontology:measurement(hcr_base_extract_1986, hindu_codified_reading, base_extractiveness, 30, 0.39).
narrative_ontology:measurement(hcr_base_extract_2005, hindu_codified_reading, base_extractiveness, 49, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(hcr_suppress_1956, hindu_codified_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(hcr_suppress_1976, hindu_codified_reading, suppression_requirement, 20, 0.48).
narrative_ontology:measurement(hcr_suppress_2005, hindu_codified_reading, suppression_requirement, 49, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hindu_codified_reading, attachment_coordination).
narrative_ontology:boltzmann_floor_override(hindu_codified_reading, 0.12).
narrative_ontology:affects_constraint(hindu_codified_reading, muslim_shariat_reading).
narrative_ontology:affects_constraint(hindu_codified_reading, secular_contractual_reading).
narrative_ontology:affects_constraint(hindu_codified_reading, christian_colonial_reading).
narrative_ontology:affects_constraint(hindu_codified_reading, parsi_community_reading).
narrative_ontology:affects_constraint(hindu_codified_reading, interfaith_marriage_access).
narrative_ontology:affects_constraint(hindu_codified_reading, female_succession_asymmetry).

% DUAL FORMULATION NOTE:
% Hindu codified marriage law is part of a constraint family modeling the marriage_authority_kernel. Each sibling reading (muslim_shariat, secular_contractual, christian_colonial, parsi_community) is a separate constraint with distinct ε values and authority structures. They are linked by the underlying kernel dispute: what is the source of marriage authority in multi-religious India? This reading (hindu_codified) grounds authority in codified dharmashastra; siblings ground it differently. The sibling relationships are bidirectional: this reading influences and is influenced by each sibling. Downstream constraints (interfaith_marriage_access, female_succession_asymmetry) are specific asymmetries that emerge from this reading's operation and are constrained by it. See constraint family documentation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(hindu_codified_reading, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
