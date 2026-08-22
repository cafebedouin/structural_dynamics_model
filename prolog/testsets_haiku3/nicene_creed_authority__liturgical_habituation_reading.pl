% ============================================================================
% CONSTRAINT STORY: nicene_creed_authority__liturgical_habituation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nicene_creed_authority__liturgical_habituation_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: nicene_creed_authority__liturgical_habituation_reading
 *   human_readable: Nicene Creed as Liturgical Identity Boundary (Habituation Reading)
 *   domain: systematic_theology/ecclesiology/liturgical_practice
 *
 * SUMMARY:
 *   The Nicene Creed (formalized 325 CE) originated as a doctrinal boundary
 *   against Arian and other Christological heterodoxies. This constraint
 *   story instantiates the LITURGICAL HABITUATION reading: the creed
 *   functions primarily as a performative identity marker in communal
 *   worship, independent of the metaphysical assent it nominally demands. The
 *   reader adopts this position: recitation in the eucharistic assembly marks
 *   membership and orthodox standing regardless of what cognitive commitments
 *   (or confusions) participants hold about substance, essence, or
 *   incarnational metaphysics. Extractiveness is minimal because the
 *   arrangement imposes little coercive overhead—participation is largely
 *   voluntary, alternatives exist (non-recitation, schism, private belief),
 *   and the creed's binding force rests on the social reward of community
 *   membership rather than heresy sanction. Theater is high (0.72) because
 *   the creed's primary work is performative—it is RECITED, not interrogated;
 *   what matters is that the words are spoken together, not that assent to
 *   their propositions is verified. The constraint feeds sibling readings by
 *   providing the social-ritual substrate on which both strict doctrinal
 *   enforcement and pluralist reinterpretation can operate.
 *
 * KEY AGENTS:
 *   - Liturgical practitioners (beneficiaries): recite the creed as communal identity marker; their assent is to participation, not necessarily to metaphysics.
 *   - Ecclesiastical hierarchy (agenda-setter): prescribes and administers the creed within rubrical discipline; enforces through performance rules, not inquisition.
 *   - Doctrinal purists (payers): believe metaphysical truth is binding; experience the habituation reading as a degradation of doctrinal authority.
 *   - Theological pluralists (beneficiary/observer): maintain diverse metaphysical positions while using the creed as performative boundary.
 *   - Historical scholars (observer): document what the creed actually regulated in practice across periods.
 *   - Converts and catechumens (excluded): systematically absent from recitation until baptism; would benefit from clarity on whether metaphysical assent is required.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nicene_creed_authority__liturgical_habituation_reading, 0.08).
domain_priors:suppression_score(nicene_creed_authority__liturgical_habituation_reading, 0.12).
domain_priors:theater_ratio(nicene_creed_authority__liturgical_habituation_reading, 0.72).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nicene_creed_authority__liturgical_habituation_reading, extractiveness, 0.08).
narrative_ontology:constraint_metric(nicene_creed_authority__liturgical_habituation_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(nicene_creed_authority__liturgical_habituation_reading, theater_ratio, 0.72).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nicene_creed_authority__liturgical_habituation_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(nicene_creed_authority__liturgical_habituation_reading, resistance, 0.28).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nicene_creed_authority__liturgical_habituation_reading, rope).
narrative_ontology:human_readable(nicene_creed_authority__liturgical_habituation_reading, "Nicene Creed as Liturgical Identity Boundary (Habituation Reading)").
narrative_ontology:topic_domain(nicene_creed_authority__liturgical_habituation_reading, "systematic_theology/ecclesiology/liturgical_practice").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nicene_creed_authority__liturgical_habituation_reading, '11669037-ee30-40aa-b426-71ee457c6a51').
narrative_ontology:cs_kernel_codification('11669037-ee30-40aa-b426-71ee457c6a51', fixed_text).
narrative_ontology:cs_authority_grounding('11669037-ee30-40aa-b426-71ee457c6a51', lineage).
narrative_ontology:cs_interpretation_layer_present('11669037-ee30-40aa-b426-71ee457c6a51').
narrative_ontology:cs_reading_relation('11669037-ee30-40aa-b426-71ee457c6a51', nicene_creed_authority__strict_orthodox_reading, coexists_with).
narrative_ontology:cs_reading_relation('11669037-ee30-40aa-b426-71ee457c6a51', nicene_creed_authority__symbolic_confessional_reading, coexists_with).
narrative_ontology:cs_axiom('11669037-ee30-40aa-b426-71ee457c6a51', foundational, creed_as_performative_boundary).
narrative_ontology:cs_axiom_status(creed_as_performative_boundary, holdable).
narrative_ontology:cs_axiom_grounding('11669037-ee30-40aa-b426-71ee457c6a51', creed_as_performative_boundary, conventional).
narrative_ontology:cs_axiom('11669037-ee30-40aa-b426-71ee457c6a51', foundational, metaphysical_assent_decoupled_from_community_membership).
narrative_ontology:cs_axiom_status(metaphysical_assent_decoupled_from_community_membership, holdable).
narrative_ontology:cs_axiom_grounding('11669037-ee30-40aa-b426-71ee457c6a51', metaphysical_assent_decoupled_from_community_membership, empirically_contingent).
narrative_ontology:cs_reference_frame('11669037-ee30-40aa-b426-71ee457c6a51', liturgical_consensus_through_recitation).
narrative_ontology:cs_drift_state('11669037-ee30-40aa-b426-71ee457c6a51', contemporary_theological_pluralism, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('11669037-ee30-40aa-b426-71ee457c6a51', '').
narrative_ontology:cs_kernel_id(nicene_creed_authority__liturgical_habituation_reading, nicene_creed_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nicene_creed_authority__liturgical_habituation_reading, liturgical_practitioners).
narrative_ontology:constraint_beneficiary(nicene_creed_authority__liturgical_habituation_reading, ecclesial_community_coherence).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(nicene_creed_authority__liturgical_habituation_reading, theological_pluralists).
narrative_ontology:constraint_victim(nicene_creed_authority__liturgical_habituation_reading, doctrinal_purists).
narrative_ontology:constraint_vindicates(nicene_creed_authority__liturgical_habituation_reading, liturgy_as_primary_identity_vehicle).
narrative_ontology:constraint_vindicates(nicene_creed_authority__liturgical_habituation_reading, doctrinal_content_substrate_independent).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Participate regularly in eucharistic liturgy, recite the Nicene Creed as a communal speech act. The creed functions as a boundary marker—reciting it places them inside the practicing community, regardless of their metaphysical assent or comprehension of disputed terms. They benefit from the social cohesion and embodied ritual regularity; the creed's cognitive content is secondary to its performative function.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__liturgical_habituation_reading, liturgical_practitioners, beneficiary,
    moderate, biographical, constrained, global).

% Prescribes and administers the creed as a liturgical standard. Sets the rubric, controls when and how it is recited, trains clergy in its performance. Their interest is in maintaining the arrangement—the creed as a lived, performed boundary that requires no heresy tribunal, because heterodox metaphysics are screened out by simple non-participation in recitation. They enforce the creed through rubrical discipline, not doctrinal interrogation.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__liturgical_habituation_reading, ecclesiastical_hierarchy, agenda_setter,
    institutional, generational, mobile, global).

% Believe the creed articulates binding metaphysical truth—that assent to its ontology is non-negotiable for Christian identity. They experience the liturgical-habituation reading as a degradation: if the creed functions merely as a boundary marker independent of metaphysical commitment, the reading appears to hollow out the doctrinal mandate they see as essential. They are the payers in the sense that their theological labor and institutional authority is subordinated to a practice (recitation) that no longer guarantees ontological consensus. Their exit is largely identity-locked: breaking with the institutional church over its permissiveness toward internal metaphysical pluralism carries social and vocational costs.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__liturgical_habituation_reading, doctrinal_purists, payer,
    organized, generational, identity_locked, global).

% Maintain diverse metaphysical positions while participating in the same liturgy. The liturgical-habituation reading permits them to recite the creed as a performative commitment to the community's identity and historical witness without requiring metaphysical assent to every proposition. They benefit from the reading's decoupling: identity can be marked without requiring cognitive agreement on what 'substance' or 'consubstantial' mean. They observe the tension between doctrinal purists and the actual pluralism the reading describes.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__liturgical_habituation_reading, theological_pluralists, beneficiary,
    moderate, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(nicene_creed_authority__liturgical_habituation_reading, theological_pluralists, observer).

% Investigate the creed's function in lived practice vs. its declared metaphysical binding force. They produce evidence (patristic texts, liturgical manuscripts, congregational testimony) about what the creed actually regulated in different historical periods and contexts. They are analytical observers: their work feeds the reading's plausibility but they do not depend on any particular interpretation for their institutional standing.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__liturgical_habituation_reading, historical_scholars, observer,
    organized, generational, analytical, global).

% Are systematically excluded from eucharistic recitation until baptism and catechesis. They would benefit from understanding whether the creed binds them metaphysically or merely marks boundary participation. The exclusion structure depends partly on the unstated answer to this question—if the creed is performative boundary-marking, the catechesis could be minimal; if it is metaphysical binding, catechesis becomes doctrinal instruction. Their voices are suppressed by the category distinction between full members and candidates.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__liturgical_habituation_reading, converts_and_catechumens, excluded,
    powerless, biographical, trapped, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Marks the boundary of the eucharistic community and episcopal authority through shared performative utterance (recitation in the liturgy). Enables scattered congregations, across centuries and regions, to recognize each other as in communion through a common creedal formula. Solves the coordination problem of Christian identity without requiring consensus on the metaphysical content the formula expresses.
% TRANSFER_FUNCTION: Transfers the authority to set community boundaries from individual heterodox teachers (who can claim private revelation or novel interpretation) to the institutional church (bishops, councils, transmitted tradition). The creed moves authority upward in the institutional hierarchy and localizes it in rubrical performance rather than distributed doctrinal interrogation. Priests, not theologians, enforce the boundary by admitting the creed-reciter to the eucharist.
% ABSENT_VOICES: Converts and catechumens are structurally excluded from recitation until baptism; they would have strong interest in clarity about whether the creed binds metaphysical truth or merely marks identity, because that distinction determines the content of their catechesis. Doctrinal theologians who might argue the creed is historically contingent rather than eternally binding are suppressed by the rubrical framework (creeds are RECITED, not debated in the liturgy; theological disputation happens elsewhere). Lay practitioners whose interior theology diverges from creedal language are structurally invisible—their dissent is not suppressed because they simply comply with the performative requirement without public assent.
% DISAPPEARANCE_RATIONALE: If the Nicene Creed disappeared overnight as a liturgical standard, the eucharistic assembly would lose its primary external marker of communal identity. Parishes would either adopt alternative creedal formulas (as Protestant traditions have) or abandon creedal recitation entirely (as some contemporary communities have). Institutional authority structures built on creedal consensus—the theological coherence of councils, the recognized standing of bishops who could exclude heretics—would lose their traditional basis. Both stricter and more permissive readings would lose the substrate on which they coexist; communities would be forced to choose: either explicit doctrinal enforcement (strictly orthodox) or explicit repudiation (pluralist). The world would rearrange because the creed, in the habituation reading, solves the problem of maintaining institutional unity across metaphysical diversity without any party explicitly choosing that diversity.
% FOUNDING_PROBLEM: In the 4th century, active Christological heterodoxies (Arianism, Docetism, other non-Nicene positions) threatened episcopal communion and doctrinal coherence. The Council of Nicaea formulated a creed to exclude these movements and establish a unified metaphysical standard. The problem was live: heterodox bishops were present in church councils, heterodox teachers were competing for influence, and the creed was designed to resolve the dispute by binding all bishops to one formulation.
% FOUNDING_PROBLEM_CORROBORATION: Patristic historians and historical theologians (outside the benefiting parties of any living reading) attest that by the medieval period, Arianism as an institutional force had ceased to exist and the creedal formulas had become universally accepted. Contemporary ecumenical theology across traditions (Orthodox, Catholic, Protestant) acknowledges the Nicene Creed as received tradition, not as a live point of dispute. The founding problem—active heterodoxy claiming episcopal standing—is not present in any major Christian tradition today. Doctrinal purists acknowledge the metaphysical content of the creed remains important, but they do not claim it functions as an active boundary against live heterodox movements. The unanimity on the creed's validity obscures the question of its function: the problem it was built to solve is gone, but the creed persists.
narrative_ontology:disappearance_verdict(nicene_creed_authority__liturgical_habituation_reading, world_rearranges).
narrative_ontology:founding_problem_status(nicene_creed_authority__liturgical_habituation_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nicene_creed_authority__liturgical_habituation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(nicene_creed_authority__liturgical_habituation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nicene_creed_authority__liturgical_habituation_reading, 0.08, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nicene_creed_authority__liturgical_habituation_reading_tests).
:- end_tests(nicene_creed_authority__liturgical_habituation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   This reading lowers extractiveness to 0.08 because the creed's operation in the liturgical-habituation frame requires minimal coercive overhead. Participants recite it because they belong to the community, not because they fear sanction for doctrinal error. The spatial scope is global (creeds are recited across all major Christian traditions) but the enforcement is local-ritual (performed at each eucharist). Suppression is low (0.12) because the creed itself does not suppress alternatives; alternatives (heterodox belief, private theology) coexist inside the community—the creed simply marks a boundary that makes heterodoxy invisible or irrelevant to institutional standing. Theater is very high (0.72) because the creed's primary function is performative liturgical speech; the theological labor involved in verifying metaphysical assent is minimal compared to the work of recitation. Accessibility collapse is low (0.35) because participants retain alternatives: they can refuse to recite (schism, attendance elsewhere), they can recite without assent (interior dissent), or they can reinterpret terms. The liturgical-habituation reading does not collapse alternatives—it merely makes recitation the boundary marker, leaving interior metaphysics a private matter. Resistance is low (0.28) because the arrangement is largely unresisted by beneficiaries; doctrinal purists resist, but they represent a minority voice within living communities.
 *
 * PERSPECTIVAL GAP:
 *   The strict_orthodox_reading (sibling constraint) computes very differently from the liturgical seat because it couples identity to metaphysical binding: from the orthodox perspective, the creed's function is doctrinal enforcement, not identity marking. The hierarchical seat experiences this as extractive (their authority to bind doctrine is contested) rather than beneficial. The theological pluralists experience the strict_orthodox reading as suppressive (they must either assent or schism), whereas the liturgical_habituation reading permits coexistence. The engine computes per-seat classification from structural data; the sibling reading produces a different beneficiary/victim structure and much higher extractiveness (likely snare or tangled_rope from the purist seat), which is the measurement the corpus exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   The liturgical practitioners sit near the beneficiary end (d ≈ 0.2): they benefit from community identity, rhythmic participation, and the social cohesion the creed enables, with minimal cost. The ecclesiastical hierarchy sits symmetrically (d ≈ 0.45)—they administer the creed but also depend on its binding force to maintain institutional coherence; their interest is in preserving the arrangement, which carries costs of maintaining rubrical discipline. Doctrinal purists are the target (d ≈ 0.75): they experience the reading as undermining their authority to bind metaphysical truth; their labor (theological justification of creedal propositions) is subordinated to a practice (recitation) that no longer guarantees doctrinal consensus. Theological pluralists sit near the beneficiary end (d ≈ 0.15): the reading permits them to participate fully in the community while maintaining interior dissent. The directionality derivation flows from the structural fact that the liturgical-habituation reading decouples identity from metaphysics—those invested in coupling pay the cost (doctrinal purists), while those who benefit from the decoupling (pluralists, ordinary practitioners) collect the benefit of community without cognitive burden.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing heterodoxy in Christology, particularly Arianism) was live at t=325. By t=1100, in most regional churches, the problem was either resolved (Arianism was vanquished as an institutional force) or transformed (the locus of heterodoxy shifted—trinitarian Nicene consensus became the baseline, and new disputes emerged about Christological hypostasis, will, nature). The liturgical_habituation reading describes a constraint whose founding problem is DEAD: the creed no longer functions primarily to exclude active heterodox movements, but rather as a performed identity boundary that can accommodate interior theological diversity. The theater ratio rising from 0.35 to 0.72 over the interval confirms this: as the metaphysical battle receded, the creed's function shifted from doctrinal barrier (functional work) to liturgical recitation (performative work). The creed persists not because it continues to solve the problem it was built for, but because it now solves a different problem: marking community coherence and episcopal authority independent of metaphysical agreement. This is a textbook mandatrophy—the arrangement continues because it has become institutionally embedded and provides secondary benefits (identity, community rhythm, historical continuity) that make fixing it costly and disruptive, even though its primary function has atrophied.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    performative_vs_propositional_semantics,
    'What semantics govern the creed''s authority—is recitation a propositional assertion (the speaker assents to the sentences'' truth conditions) or a performative act (the speaker commits to community identity through the utterance)?',
    'Linguistic anthropology of creedal recitation in living communities: do practitioners report their recitation as doctrinal assertion or as community boundary-marking? Do they experience interior dissent as incompatible with recitation? Patristic analysis of original creedal contexts: were creeds formulated as doctrinal tests or as liturgical unity markers?',
    'If primarily propositional, extractiveness rises and the strict_orthodox_reading gains structural plausibility. If primarily performative, the liturgical_habituation reading is the structural ground and other readings are secondary framings of the same social practice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(performative_vs_propositional_semantics, conceptual, 'Linguistic and performative semantics of creedal authority.').

omega_variable(
    metaphysical_plurality_within_tradition,
    'To what extent have Christian communities historically maintained metaphysical disagreement about substance, nature, will, and hypostasis while remaining in formal communion through shared creedal recitation?',
    'Patristic source examination: do Eastern and Western churches disagree on metaphysical interpretation while both reciting the Nicene Creed? Do early medieval monastic communities contain theological debate alongside common liturgical recitation? Do contemporary Orthodox and Catholic and Protestant communities hold divergent metaphysical interpretations while sharing creedal language?',
    'Clear evidence of sustained metaphysical plurality within communion would confirm the liturgical_habituation reading''s structural claim: the creed marks boundary while permitting interior diversity. Absence would suggest the creed functioned primarily as doctrinal enforcement, favoring the strict_orthodox reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(metaphysical_plurality_within_tradition, empirical, 'Historical extent of metaphysical plurality within liturgical unity.').

omega_variable(
    theater_ratio_interpretation,
    'Is the rising theater_ratio (0.35 → 0.72) evidence that the creed''s function shifted from doctrinal enforcement to performative identity marking, or does it indicate that the creed was ALWAYS performative and the theater ratio simply reflects increasing ecclesiastical formalization?',
    'Trace the history of creedal enforcement mechanisms: did early councils and bishops use creeds to interrogate assent (heresy trials, doctrinal examination)? At what historical point did enforcement mechanisms soften and recitation alone become sufficient? Did the shift correlate with changing metaphysical consensus (the heterodox problem was solved) or with changing ecclesiastical administration (professionalization of liturgy, separation of doctrinal authority from eucharistic presidency)?',
    'If the shift is causal (function changed because metaphysical problem was solved), the mandatrophy diagnosis is strong. If the shift is administrative (enforcement mechanisms changed but the function remained binding metaphysical commitment), the creed''s theater is a mask over ongoing enforcement, and the strict_orthodox reading better describes the actual constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theater_ratio_interpretation, empirical, 'Causal direction of theater-ratio change: functional shift vs. administrative formalization.').

omega_variable(
    sibling_reading_committer_frame,
    'What is the theoretical and institutional status of the three sibling readings—are they equally live positions in contemporary Christianity, or does one reading''s institutional dominance suppress the others?',
    'Institutional survey: which reading dominates in each tradition (Catholic, Orthodox, Protestant mainline, evangelical)? Do traditions that officially endorse one reading (e.g., Catholic metaphysical binding) informally practice another (e.g., permitting interior theological diversity)? Do academic theologians and parish practitioners diverge in their endorsed reading?',
    'If one reading is institutionally dominant and enforced, it becomes the effective constraint despite others being theoretically available. If readings coexist across different seats (stricter in some traditions, more permissive in others), the constraint family is genuinely multivalent and the sibling constraints have different structural positions in different institutional contexts.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_committer_frame, empirical, 'Institutional distribution and dominance of the three creedal readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nicene_creed_authority__liturgical_habituation_reading, 325, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nice_tr_t325, nicene_creed_authority__liturgical_habituation_reading, theater_ratio, 325, 0.35).
narrative_ontology:measurement_basis(nice_tr_t325, projected).
narrative_ontology:measurement(nice_tr_t600, nicene_creed_authority__liturgical_habituation_reading, theater_ratio, 600, 0.48).
narrative_ontology:measurement_basis(nice_tr_t600, observed).
narrative_ontology:measurement(nice_tr_t1100, nicene_creed_authority__liturgical_habituation_reading, theater_ratio, 1100, 0.62).
narrative_ontology:measurement_basis(nice_tr_t1100, observed).
narrative_ontology:measurement(nice_tr_t1600, nicene_creed_authority__liturgical_habituation_reading, theater_ratio, 1600, 0.68).
narrative_ontology:measurement_basis(nice_tr_t1600, observed).
narrative_ontology:measurement(nice_tr_t1850, nicene_creed_authority__liturgical_habituation_reading, theater_ratio, 1850, 0.7).
narrative_ontology:measurement_basis(nice_tr_t1850, observed).
narrative_ontology:measurement(nice_tr_t2000, nicene_creed_authority__liturgical_habituation_reading, theater_ratio, 2000, 0.71).
narrative_ontology:measurement_basis(nice_tr_t2000, observed).
narrative_ontology:measurement(nice_tr_t2026, nicene_creed_authority__liturgical_habituation_reading, theater_ratio, 2026, 0.72).
narrative_ontology:measurement_basis(nice_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(nice_be_t325, nicene_creed_authority__liturgical_habituation_reading, base_extractiveness, 325, 0.15).
narrative_ontology:measurement_basis(nice_be_t325, projected).
narrative_ontology:measurement(nice_be_t600, nicene_creed_authority__liturgical_habituation_reading, base_extractiveness, 600, 0.12).
narrative_ontology:measurement_basis(nice_be_t600, observed).
narrative_ontology:measurement(nice_be_t1100, nicene_creed_authority__liturgical_habituation_reading, base_extractiveness, 1100, 0.08).
narrative_ontology:measurement_basis(nice_be_t1100, observed).
narrative_ontology:measurement(nice_be_t1600, nicene_creed_authority__liturgical_habituation_reading, base_extractiveness, 1600, 0.06).
narrative_ontology:measurement_basis(nice_be_t1600, observed).
narrative_ontology:measurement(nice_be_t1850, nicene_creed_authority__liturgical_habituation_reading, base_extractiveness, 1850, 0.08).
narrative_ontology:measurement_basis(nice_be_t1850, observed).
narrative_ontology:measurement(nice_be_t2000, nicene_creed_authority__liturgical_habituation_reading, base_extractiveness, 2000, 0.09).
narrative_ontology:measurement_basis(nice_be_t2000, observed).
narrative_ontology:measurement(nice_be_t2026, nicene_creed_authority__liturgical_habituation_reading, base_extractiveness, 2026, 0.08).
narrative_ontology:measurement_basis(nice_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(nice_su_t325, nicene_creed_authority__liturgical_habituation_reading, suppression_requirement, 325, 0.45).
narrative_ontology:measurement_basis(nice_su_t325, projected).
narrative_ontology:measurement(nice_su_t600, nicene_creed_authority__liturgical_habituation_reading, suppression_requirement, 600, 0.32).
narrative_ontology:measurement_basis(nice_su_t600, observed).
narrative_ontology:measurement(nice_su_t1100, nicene_creed_authority__liturgical_habituation_reading, suppression_requirement, 1100, 0.18).
narrative_ontology:measurement_basis(nice_su_t1100, observed).
narrative_ontology:measurement(nice_su_t1600, nicene_creed_authority__liturgical_habituation_reading, suppression_requirement, 1600, 0.12).
narrative_ontology:measurement_basis(nice_su_t1600, observed).
narrative_ontology:measurement(nice_su_t1850, nicene_creed_authority__liturgical_habituation_reading, suppression_requirement, 1850, 0.14).
narrative_ontology:measurement_basis(nice_su_t1850, observed).
narrative_ontology:measurement(nice_su_t2000, nicene_creed_authority__liturgical_habituation_reading, suppression_requirement, 2000, 0.12).
narrative_ontology:measurement_basis(nice_su_t2000, observed).
narrative_ontology:measurement(nice_su_t2026, nicene_creed_authority__liturgical_habituation_reading, suppression_requirement, 2026, 0.12).
narrative_ontology:measurement_basis(nice_su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nicene_creed_authority__liturgical_habituation_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(nicene_creed_authority__liturgical_habituation_reading, 0.06).
narrative_ontology:affects_constraint(nicene_creed_authority__liturgical_habituation_reading, nicene_creed_authority__strict_orthodox_reading).
narrative_ontology:affects_constraint(nicene_creed_authority__liturgical_habituation_reading, nicene_creed_authority__symbolic_confessional_reading).

% DUAL FORMULATION NOTE:
% The nicene_creed_authority kernel decomposes into three structurally distinct constraint readings, each with different ε, beneficiary/victim structure, and type. The liturgical_habituation_reading (this story) describes the actual lived practice—recitation in eucharistic assembly—independent of metaphysical commitment. The strict_orthodox_reading describes the same creed as binding metaphysical doctrine, with much higher extractiveness and different victim structure (heterodox believers). The symbolic_confessional_reading describes it as historical witness grounded in community discernment. All three read the same kernel (the Nicene Creed as authoritative text), but each instantiates a different constraint with different structural properties. The liturgical reading provides the social substrate on which the other two operate—it is the ground condition that permits both stricter enforcement and more permissive interpretation to coexist in lived communities.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
