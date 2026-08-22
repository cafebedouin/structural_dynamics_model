% ============================================================================
% CONSTRAINT STORY: nicene_creed_authority__strict_orthodox_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nicene_creed_authority__strict_orthodox_reading, []).

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
 *   constraint_id: nicene_creed_authority__strict_orthodox_reading
 *   human_readable: Nicene Creed Doctrinal Binding (Strict Orthodox Reading)
 *   domain: religious/theological
 *
 * SUMMARY:
 *   The Nicene Creed, formalized at the Council of Nicaea (325 CE), binds all
 *   believers within the Orthodox Christian tradition to a single
 *   metaphysical ontology regarding Christ's nature and divinity. This
 *   constraint instantiates the strict Orthodox reading: the creed is not a
 *   guideline or symbolic boundary, but a binding doctrinal requirement whose
 *   violation constitutes heresy warranting institutional sanction. The
 *   constraint exhibits high extractiveness (0.68): the beneficiary clergy
 *   controls both the creed's interpretation and the enforcement apparatus,
 *   while heterodox communities and dissenting theologians bear the costs of
 *   suppression. The constraint is actively enforced through councils,
 *   excommunication, and civil persecution. This JSON documents the strict
 *   Orthodox reading's structural properties as one interpretation of the
 *   contested kernel 'nicene_creed_authority'; it is NOT a claim about which
 *   reading is historically primary or epistemically correct. The sibling
 *   readings (liturgical_habituation, symbolic_confessional) are other
 *   constraints in the same family, each instantiating a different
 *   interpretation of the kernel.
 *
 * KEY AGENTS:
 *   - hierarchical_clergy: institutional beneficiary (d≈0.1); sets and enforces creedal doctrine; power flows to this seat
 *   - heterodox_communities: powerless victims (d≈0.95); suppressed, subject to sanction; trapped exit
 *   - lay_interpreters: moderate victims (d≈0.75); excluded from authority-setting; constrained exit
 *   - ecumenical_councils: institutional agenda-setter (d≈0.2); codification and enforcement mechanism
 *   - believing_laity_conformist: moderate dual agent (d≈0.45); beneficiaries of community/security, but identity-locked to creedal conformity
 *   - imperial_authorities: institutional beneficiary (d≈0.05); leverage doctrinal uniformity for civil stability
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nicene_creed_authority__strict_orthodox_reading, 0.68).
domain_priors:suppression_score(nicene_creed_authority__strict_orthodox_reading, 0.76).
domain_priors:theater_ratio(nicene_creed_authority__strict_orthodox_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nicene_creed_authority__strict_orthodox_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(nicene_creed_authority__strict_orthodox_reading, suppression_requirement, 0.76).
narrative_ontology:constraint_metric(nicene_creed_authority__strict_orthodox_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nicene_creed_authority__strict_orthodox_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(nicene_creed_authority__strict_orthodox_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nicene_creed_authority__strict_orthodox_reading, tangled_rope).
narrative_ontology:human_readable(nicene_creed_authority__strict_orthodox_reading, "Nicene Creed Doctrinal Binding (Strict Orthodox Reading)").
narrative_ontology:topic_domain(nicene_creed_authority__strict_orthodox_reading, "religious/theological").

domain_priors:requires_active_enforcement(nicene_creed_authority__strict_orthodox_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nicene_creed_authority__strict_orthodox_reading, '52d47004-3ab5-4914-8424-0eba1af4a8b4').
narrative_ontology:cs_kernel_codification('52d47004-3ab5-4914-8424-0eba1af4a8b4', fixed_text).
narrative_ontology:cs_authority_grounding('52d47004-3ab5-4914-8424-0eba1af4a8b4', extraction).
narrative_ontology:cs_interpretation_layer_present('52d47004-3ab5-4914-8424-0eba1af4a8b4').
narrative_ontology:cs_reading_relation('52d47004-3ab5-4914-8424-0eba1af4a8b4', nicene_creed_authority__liturgical_habituation_reading, influences).
narrative_ontology:cs_reading_relation('52d47004-3ab5-4914-8424-0eba1af4a8b4', nicene_creed_authority__symbolic_confessional_reading, coexists_with).
narrative_ontology:cs_axiom('52d47004-3ab5-4914-8424-0eba1af4a8b4', foundational, binding_metaphysical_realism).
narrative_ontology:cs_axiom_status(binding_metaphysical_realism, holdable).
narrative_ontology:cs_axiom_grounding('52d47004-3ab5-4914-8424-0eba1af4a8b4', binding_metaphysical_realism, deontological).
narrative_ontology:cs_axiom('52d47004-3ab5-4914-8424-0eba1af4a8b4', foundational, heresy_warranting_sanction).
narrative_ontology:cs_axiom_status(heresy_warranting_sanction, holdable).
narrative_ontology:cs_axiom_grounding('52d47004-3ab5-4914-8424-0eba1af4a8b4', heresy_warranting_sanction, conventional).
narrative_ontology:cs_reference_frame('52d47004-3ab5-4914-8424-0eba1af4a8b4', apostolic_orthodoxy_standard).
narrative_ontology:cs_drift_state('52d47004-3ab5-4914-8424-0eba1af4a8b4', contemporary_ecumenical_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('52d47004-3ab5-4914-8424-0eba1af4a8b4', '').
narrative_ontology:cs_kernel_id(nicene_creed_authority__strict_orthodox_reading, nicene_creed_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nicene_creed_authority__strict_orthodox_reading, hierarchical_clergy).
narrative_ontology:constraint_beneficiary(nicene_creed_authority__strict_orthodox_reading, orthodox_institutional_church).
narrative_ontology:constraint_victim(nicene_creed_authority__strict_orthodox_reading, heterodox_communities).
narrative_ontology:constraint_victim(nicene_creed_authority__strict_orthodox_reading, lay_interpreters).
narrative_ontology:constraint_victim(nicene_creed_authority__strict_orthodox_reading, dissenting_theologians).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(nicene_creed_authority__strict_orthodox_reading, ecumenical_councils).
narrative_ontology:constraint_beneficiary(nicene_creed_authority__strict_orthodox_reading, imperial_authorities).
narrative_ontology:constraint_beneficiary(nicene_creed_authority__strict_orthodox_reading, believing_laity_conformist).
narrative_ontology:constraint_victim(nicene_creed_authority__strict_orthodox_reading, believing_laity_conformist).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bishops and church hierarchy set and enforce creedal interpretation as binding doctrine. Their authority is grounded in claim of apostolic succession and custodianship of orthodox truth. They maintain the creed through councils, synods, and disciplinary mechanisms (excommunication, anathema). Non-compliance with creedal uniformity directly threatens their institutional authority and the legitimacy structure they occupy.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__strict_orthodox_reading, hierarchical_clergy, agenda_setter,
    institutional, generational, mobile, continental).

% The formalized church institution benefits from doctrinal uniformity as a boundary-maintenance and identity-consolidation mechanism. Coherent creedal doctrine allows unified institutional action, claims to universal truth, and justification of hierarchy. The creed becomes the church's primary asset in defining its scope and authority.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__strict_orthodox_reading, orthodox_institutional_church, beneficiary,
    institutional, civilizational, arbitrage, global).

% Communities holding non-conforming theological interpretations (Arian, Gnostic, Nestorian, or other heterodox positions) are subject to sanction: exclusion from sacraments, excommunication, suppression of texts, social ostracism, and in various periods and regions, civil persecution. Their ability to maintain alternative theological frameworks depends on concealment or geographic isolation. Exit means loss of community, identity, and spiritual authority within the only tradition available to them.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__strict_orthodox_reading, heterodox_communities, payer,
    powerless, biographical, trapped, local).

% Laypeople and non-ordained believers who arrive at heterodox interpretations through personal study or conviction are prohibited from teaching or promulgating those interpretations. Their private belief may be tolerated, but public expression risks sanction. They are excluded from doctrinal authority-setting while being bound by its outcomes. Their voice in defining orthodoxy is denied structurally.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__strict_orthodox_reading, lay_interpreters, payer,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(nicene_creed_authority__strict_orthodox_reading, lay_interpreters, excluded).

% Trained theologians who challenge creedal interpretation or propose alternative metaphysical framings face professional sanction: loss of teaching position, exclusion from councils, labeling as heretic. Their scholarly voice is suppressed through institutional leverage. They retain some power through intellectual networks and manuscript production, but face systematic barriers to legitimacy and resources.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__strict_orthodox_reading, dissenting_theologians, payer,
    organized, biographical, constrained, regional).

% Formal gatherings (Nicaea 325 CE, Constantinople 381, Chalcedon 451, etc.) of bishops that codify and enforce creedal doctrine. These councils function as the enforcement apparatus and legitimacy factory for the constraint. They produce binding decrees, anathematize heretics, and authorize sanctions. Council participation is itself restricted to approved clergy, creating a self-reinforcing enforcement loop.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__strict_orthodox_reading, ecumenical_councils, agenda_setter,
    institutional, generational, mobile, continental).
narrative_ontology:stakeholder_secondary_role(nicene_creed_authority__strict_orthodox_reading, ecumenical_councils, beneficiary).

% Civil authorities (Roman emperors, later Christian states) benefit from doctrinal uniformity as a stability mechanism. Religious uniformity reduces internal conflict and strengthens state control. Imperial enforcement of creedal conformity extends the church's reach into civil law, making heresy a civil crime. The state leverages the creed as a tool of social cohesion and borrows religious legitimacy.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__strict_orthodox_reading, imperial_authorities, beneficiary,
    institutional, generational, mobile, continental).

% The majority of believers who accept creedal orthodoxy gain spiritual security (assurance of correct salvation), community belonging, and institutional protection. They also incur the diffuse cost of cognitive closure: they surrender independent theological inquiry and risk internal doubt about conforming to a received doctrine. Their identity as 'orthodox believer' becomes fused with creedal adherence, making exit psychologically costly.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__strict_orthodox_reading, believing_laity_conformist, beneficiary,
    moderate, biographical, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(nicene_creed_authority__strict_orthodox_reading, believing_laity_conformist, payer).

% Non-Nicene Christian communities (Coptic, Syrian, Persian, and other Eastern churches; later Reformation movements) are structurally barred from creedal authority-setting and face delegitimization. Their theological vocabularies and metaphysical frameworks are branded as heresy rather than engaged as alternative interpretations. They survive through institutional separation, not through inclusion or dialogue.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__strict_orthodox_reading, alternative_theological_traditions, excluded,
    organized, generational, trapped, regional).

% Schools and catechetical centers that train clergy in orthodox doctrine. They institutionalize creedal interpretation, police its transmission, and produce the next generation of enforcers. They benefit from doctrinal stability (curriculum is fixed, authority is uncontested) while suppressing heterodox scholarship.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__strict_orthodox_reading, theological_academies, agenda_setter,
    institutional, generational, mobile, regional).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Unifies a geographically dispersed community of believers under a shared metaphysical framework, enabling institutional coordination, sacramental validity across communities, and collective theological authority. The creed solves the collective-action problem of defining who belongs to the church and on what doctrinal terms.
% TRANSFER_FUNCTION: Moves interpretive authority from individual believers and lay communities to hierarchical clergy. Moves theological voice from dissenting scholars and heterodox communities to approved councils and institutional magisterium. Moves spiritual legitimacy from alternative Christian communities to the Nicene institutional church. Moves conformity cost (cognitive closure, suppression of inquiry, identity fusion) from hierarchical beneficiaries to targeted victim groups.
% ABSENT_VOICES: Heterodox Christian communities are structurally excluded (not present at councils, subject to anathema). Lay believers who privately question doctrine are silenced by institutional suppression. Women theologians and non-clerical thinkers are excluded from creedal authority-setting. Alternative Eastern Christian traditions (Coptic, Syrian, Nestorian) are present in history but delegitimized as heretical voices rather than heard as participants.
% DISAPPEARANCE_RATIONALE: If the Nicene creedal binding and its enforcement apparatus vanished, the church would reorganize around alternative authority structures (scripture, local councils, mystical experience, charismatic authority) or fragment into autonomous theological communities. The institutional hierarchy's claim to universal doctrinal authority would collapse. Suppressed heterodox traditions would resurface. Theological inquiry would decentralize. The church's institutional identity would require reimagination.
% FOUNDING_PROBLEM: Early Christian communities developed inconsistent theologies of Christ's nature, divinity, and relationship to the Father (Arianism, Subordinationism, Modalism, and Docetism competed). This theological diversity threatened institutional coherence and created competing claims to apostolic legitimacy. The constraint was built to establish a single binding metaphysical standard that all believers must profess.
% FOUNDING_PROBLEM_CORROBORATION: Orthodox hierarchical churches attest the founding problem remains live: theological coherence and sacramental validity still require creedal uniformity. Heterodox traditions, modern exegetical scholars, and secular historians of Christianity attest the founding problem has been substantially reformulated: theological diversity need not threaten institutional identity; the persistence of creedal binding is better explained as institutional rent-seeking and identity-consolidation than as necessary coordination. Sociological and historical analysis outside the benefiting parties documents that alternative Christian communities thrived for centuries without Nicene creedal binding.
narrative_ontology:disappearance_verdict(nicene_creed_authority__strict_orthodox_reading, world_rearranges).
narrative_ontology:founding_problem_status(nicene_creed_authority__strict_orthodox_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nicene_creed_authority__strict_orthodox_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(nicene_creed_authority__strict_orthodox_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nicene_creed_authority__strict_orthodox_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nicene_creed_authority__strict_orthodox_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(nicene_creed_authority__strict_orthodox_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(nicene_creed_authority__strict_orthodox_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness is high (0.68) because the constraint operates by restricting interpretive authority to hierarchical clergy and requiring cognitive assent to a specific metaphysical framework. The extraction is not merely coordination cost — it is the confiscation of theological voice. Suppression is higher (0.76) because the constraint's persistence depends on active enforcement: excommunication, anathema, manuscript destruction, and in many periods and regions, civil persecution. Theater ratio is moderate-high (0.42) because while genuine coordination concerns exist (early theological pluralism did create institutional friction), a growing share of enforcement activity in later centuries defended creedal uniformity as an identity marker and power consolidation mechanism rather than as a live theological necessity. The measurement series tracks increasing extractiveness through the medieval period (0 to 1000), peak suppression around 1000, and then partial decline (1500–1700) as alternative theological frameworks gained institutional foothold without collapsing the creed's formal binding power. The shared time grid ensures all three metrics are authored at every point.
 *
 * PERSPECTIVAL GAP:
 *   The engine should compute radically different types from the different seats. From the hierarchical clergy's seat, this is likely to compute as Tangled Rope: genuine coordination function (theological coherence, sacramental validity) plus asymmetric extraction (they control the interpretation, others obey). From the heterodox communities' seat, this should compute as Snare: the coordination story (unified doctrine) is cover for pure suppression of their theological voice; they are given no meaningful participation in the coordination and experience only the extraction. From the lay conformist's seat, it may compute as Tangled Rope or even Rope if they do not perceive themselves as suppressed — but the identity_locked exit option should modulate their directionality higher (more target-like) than the purely cognitive cost suggests. The divergence in computed types across seats is the measurement this story enables.
 *
 * DIRECTIONALITY LOGIC:
 *   Hierarchical clergy occupy the beneficiary end (d≈0.1–0.2): they set the creed, interpret it, enforce it, and gain institutional authority from its binding power. Their power is high (institutional) and their exit options are mobile — they can shift doctrinal interpretation within limits while maintaining their seat. Heterodox communities and dissenting theologians occupy the target end (d≈0.75–0.95): they bear the costs of suppression, face exclusion from authority-setting, and have trapped or constrained exit (losing community, professional standing, spiritual authority). The believing laity who conform sit near symmetric (d≈0.45): they genuinely benefit from coordination (community, spiritual coherence) but incur diffuse costs (cognitive closure, identity fusion that makes exit psychologically costly). Imperial authorities sit near the beneficiary end (d≈0.05–0.2) insofar as they leverage doctrinal uniformity without bearing its suppressive costs — they impose conformity on others but can exit from religious governance if it becomes administratively burdensome.
 *
 * MANDATROPHY ANALYSIS:
 *   The strict Orthodox reading CLAIMS to solve a founding problem (theological coherence) but the measurements show extractiveness accumulating over time (0.45 → 0.68) while theater ratio rises (0.25 → 0.42). This suggests the constraint's primary function has shifted: early in the interval, creedal binding was necessary for institutional survival against theological pluralism. By the medieval period, the threat was substantially contained, but the constraint persisted and intensified, now functioning primarily as a mechanism of hierarchical control and identity consolidation. The mismatch between founding_problem_status (contested) and disappearance_verdict (world_rearranges) signals mandatrophy: the constraint persists despite its founding function being disputed and substantially obsolete. The institutional beneficiaries have incentive to maintain the constraint (it consolidates their authority) even after the coordination problem it nominally solves has been resolved. This is a classic mandatrophy pattern: an institutional arrangement outlives its operational necessity and transforms into institutional rent-seeking.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    metaphysical_realism_vs_pragmatic_identity,
    'Is the creed''s binding power grounded in the metaphysical claim''s truth-value (realist grounding) or in its function as an identity boundary (pragmatic grounding)?',
    'Historical investigation: (1) Do patristic sources treat creedal conformity as requiring cognitive assent to metaphysical propositions or as ritual/community participation? (2) What happened to heterodox communities that rejected the metaphysics but maintained the liturgical practice? (3) Can modern theologians hold the creed as true while reinterpreting its metaphysical content?',
    'If realist grounding: the constraint is fundamentally about enforcing correct theology; extraction is a secondary effect of defending truth. If pragmatic grounding: the metaphysical content is instrumentally chosen; the creed functions as identity marker whose specific content is less important than its role in defining membership. The reading would modulate toward Liturgical_Habituation. If both: the strict_orthodox reading''s claim to binding metaphysics is stable, but the constraint''s persistence may be overdrawn by identity functions.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(metaphysical_realism_vs_pragmatic_identity, conceptual, 'Whether the creed''s binding force derives from metaphysical realism or pragmatic identity function.').

omega_variable(
    dissent_suppression_mechanism,
    'Is the measured suppression (0.76) structurally necessary to maintain creedal binding, or is it primarily instrumental to defending hierarchical authority?',
    'Counterfactual analysis: (1) In periods where creedal enforcement was relaxed (e.g., some medieval monasteries, Reformation-era theological pluralism), what happened to creedal coherence? (2) Could creedal authority persist through voluntary adherence (low suppression) or does it require active enforcement? (3) Are suppression mechanisms proportional to actual theological threat, or do they escalate to defend institutional hierarchy independent of theological divergence?',
    'If structurally necessary: suppression is an unavoidable cost of creedal coordination; extraction is higher but justifiable. If primarily instrumental: suppression is a choice-layer on top of coordination; it could be reduced while maintaining the creed''s authority through legitimacy rather than coercion. The constraint could modulate toward Rope. If both: the early period required suppression to establish doctrinal stability, but later periods maintained suppression primarily to defend institutional hierarchy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dissent_suppression_mechanism, empirical, 'Whether suppression is structurally necessary or instrumentally chosen.').

omega_variable(
    heterodox_invisibility_bias,
    'Does the historical record underestimate the viability and theological coherence of heterodox alternatives because suppression mechanisms successfully erased them from institutional memory?',
    'Textual archaeology: recovered heterodox manuscripts, sectarian histories, and non-institutional theological traditions reveal what suppression aimed to eliminate. Comparative analysis: alternative Christian communities (Coptic, Syrian, Persian) that developed without Nicene creedal binding — what theological problems did they solve or fail to solve? Did their theology lack coherence, or did suppression prevent integration?',
    'If heterodox alternatives were genuinely incoherent: the creed solved a real coordination problem and extractiveness is partly justified. If heterodox alternatives were suppressed despite theological viability: the constraint is primarily extractive (snare-like), and measured extractiveness understates the true extraction of theoretical voice. This would argue for reclassification or an upward revision of ε.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(heterodox_invisibility_bias, empirical, 'Whether heterodox theological alternatives were inherently inviable or suppressed despite viability.').

omega_variable(
    identity_lock_vs_voluntary_conformity,
    'For conformist believers, is creedal adherence voluntary (they genuinely assent and benefit from uniformity) or identity-locked (they cannot articulate dissent without losing community/identity)?',
    'Cognitive-frame analysis: (1) In periods of theological questioning (Reformation, modern theology), which believers shifted creedal interpretation vs. which exited the tradition entirely? (2) Interviews/testimony from believers raised in strict creedal traditions: do they report genuine assent or performed conformity? (3) What happens to conformist believers'' theology after leaving the creedal institution (post-exit drift analysis)?',
    'If genuinely voluntary: conformist believers are beneficiaries of coordination and their directionality should be d≈0.4 (symmetric). If identity-locked: their perceived benefit is obscured by psychological fusion; exit costs are higher than stated; effective d should be d≈0.6 (more target-like). The constraint''s total extraction would be higher than the nominal ε suggests because suppression of alternatives is internalized in believers.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_vs_voluntary_conformity, empirical, 'Whether conformist believers'' assent is voluntary or identity-fused.').

omega_variable(
    reading_foreclosure_contested,
    'Does the strict_orthodox reading''s core premise (binding metaphysical realism) logically foreclose the symbolic_confessional reading, or can both readings coexist in different parties'' frameworks?',
    'Formal analysis: The strict reading asserts ''the creed binds all believers to one metaphysical ontology.'' The symbolic reading asserts ''the creed is historically contingent witness; authority derives from community discernment.'' Do these premises contradict such that no single theological framework could coherently hold both? Or can a framework accept both: ''the creed expresses binding truth AND it is open to reinterpretation AND community discernment participates in its authority''?',
    'If truly contradictory: reading_relations should include foreclose. If compatible: reading_relations should include coexists_with. If one creates structural pressure on the other without logical contradiction: influences. This omega captures the under-determination in the reading-relations choice.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_foreclosure_contested, conceptual, 'Whether strict_orthodox and symbolic_confessional readings logically foreclose or coexist.').

omega_variable(
    creed_reform_vs_creed_replacement,
    'If the Nicene Creed''s enforcement apparatus were removed (sanctions lifted, councils decentralized), would the creed persist as a voluntary standard, or would alternative creeds and theological frameworks rapidly emerge?',
    'Historical analogy: (1) In Protestant Reformation aftermath, multiple confessions coexisted (Lutheran, Reformed, Catholic) when central enforcement was fragmented — did this pluralism represent genuine theological recovery or institutional collapse? (2) In modern ecumenical dialogue, do churches holding Nicene creed interpret it uniformly when enforcement is absent? (3) Among lay believers, what happens to creedal adherence when institutional pressure is removed?',
    'If creed persists under voluntary adherence: it has genuine binding power grounded in legitimate authority, not just suppression; extractiveness ε may be lower than 0.68 because much of the constraint''s function is coordination rather than pure extraction. If creed erodes rapidly: it persists primarily through institutional enforcement; extractiveness is high because suppression is the primary mechanism; this suggests ε should remain high or increase (the constraint''s true extractiveness is masked by successful suppression that makes the extraction invisible).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(creed_reform_vs_creed_replacement, empirical, 'Whether Nicene Creed would persist as voluntary standard without enforcement apparatus.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nicene_creed_authority__strict_orthodox_reading, 0, 1700).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nice_tr_t0, nicene_creed_authority__strict_orthodox_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(nice_tr_t200, nicene_creed_authority__strict_orthodox_reading, theater_ratio, 200, 0.32).
narrative_ontology:measurement(nice_tr_t500, nicene_creed_authority__strict_orthodox_reading, theater_ratio, 500, 0.41).
narrative_ontology:measurement(nice_tr_t1000, nicene_creed_authority__strict_orthodox_reading, theater_ratio, 1000, 0.44).
narrative_ontology:measurement(nice_tr_t1500, nicene_creed_authority__strict_orthodox_reading, theater_ratio, 1500, 0.4).
narrative_ontology:measurement(nice_tr_t1700, nicene_creed_authority__strict_orthodox_reading, theater_ratio, 1700, 0.42).

% Extraction over time
narrative_ontology:measurement(nice_be_t0, nicene_creed_authority__strict_orthodox_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(nice_be_t200, nicene_creed_authority__strict_orthodox_reading, base_extractiveness, 200, 0.58).
narrative_ontology:measurement(nice_be_t500, nicene_creed_authority__strict_orthodox_reading, base_extractiveness, 500, 0.68).
narrative_ontology:measurement(nice_be_t1000, nicene_creed_authority__strict_orthodox_reading, base_extractiveness, 1000, 0.71).
narrative_ontology:measurement(nice_be_t1500, nicene_creed_authority__strict_orthodox_reading, base_extractiveness, 1500, 0.66).
narrative_ontology:measurement(nice_be_t1700, nicene_creed_authority__strict_orthodox_reading, base_extractiveness, 1700, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(nice_su_t0, nicene_creed_authority__strict_orthodox_reading, suppression_requirement, 0, 0.52).
narrative_ontology:measurement(nice_su_t200, nicene_creed_authority__strict_orthodox_reading, suppression_requirement, 200, 0.68).
narrative_ontology:measurement(nice_su_t500, nicene_creed_authority__strict_orthodox_reading, suppression_requirement, 500, 0.79).
narrative_ontology:measurement(nice_su_t1000, nicene_creed_authority__strict_orthodox_reading, suppression_requirement, 1000, 0.81).
narrative_ontology:measurement(nice_su_t1500, nicene_creed_authority__strict_orthodox_reading, suppression_requirement, 1500, 0.74).
narrative_ontology:measurement(nice_su_t1700, nicene_creed_authority__strict_orthodox_reading, suppression_requirement, 1700, 0.76).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nicene_creed_authority__strict_orthodox_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(nicene_creed_authority__strict_orthodox_reading, 0.12).
narrative_ontology:affects_constraint(nicene_creed_authority__strict_orthodox_reading, nicene_creed_authority__liturgical_habituation_reading).
narrative_ontology:affects_constraint(nicene_creed_authority__strict_orthodox_reading, nicene_creed_authority__symbolic_confessional_reading).
narrative_ontology:affects_constraint(nicene_creed_authority__strict_orthodox_reading, heterodoxy_suppression__systematic_theology).
narrative_ontology:affects_constraint(nicene_creed_authority__strict_orthodox_reading, ecumenical_council_authority__medieval_period).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the kernel 'nicene_creed_authority'. The strict_orthodox_reading treats the creed as binding metaphysical doctrine enforced through heresy sanctions. The sibling readings (liturgical_habituation, symbolic_confessional) interpret the same kernel as boundary ritual and as historical witness respectively, producing structurally different constraints with different ε values, beneficiary/victim sets, and enforcement mechanisms. All three siblings must be authored separately per the ε-invariance principle; this file documents only the strict_orthodox reading. The constraint family should be linked: strict_orthodox → liturgical_habituation (influences), strict_orthodox → symbolic_confessional (influences).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
