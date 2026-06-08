% ============================================================================
% CONSTRAINT STORY: textual_authority_structure
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_textual_authority_structure, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
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
 *   constraint_id: textual_authority_structure
 *   human_readable: Textual Authority Structure in Rabbinic Temple Sacrifice Law
 *   domain: religious_law/textual_tradition/ritual_studies
 *
 * SUMMARY:
 *   The textual authority structure in rabbinic preservation of Temple
 *   sacrifice law (Kodashim) represents a constraint that crystallizes
 *   fundamental questions about how textual traditions maintain commitments
 *   when their original performance becomes structurally impossible. For
 *   approximately 1,900 years following the Temple's destruction in 70 CE,
 *   Jewish legal scholarship has preserved and continuously studied the
 *   detailed laws of Temple sacrifice — laws whose performance was deferred
 *   indefinitely. The constraint operates at the intersection of three
 *   structural tensions: (1) the hermeneutic claim that textual
 *   study-as-exercise maintains covenant fidelity versus the alternative that
 *   study is merely archival preservation; (2) the institutional authority of
 *   rabbinic consensus suppressing Karaite literalism and Reform
 *   alternatives; and (3) the identity-locked trap of the priestly lineage
 *   (Kohanim), who retain a status (privilege and obligation) that has no
 *   operational outlet. The constraint exhibits characteristics of tangled
 *   rope (coordination + enforcement) masked by false-summit naturalization
 *   (perspective 5 naturalizes the choice as inevitable). Theater ratio
 *   (0.68) reflects that the ritual apparatus — liturgical commemoration of
 *   sacrifices, theoretical legal discussion, preparation for a Temple
 *   restoration that remains deferred — has increasingly become symbolic
 *   performance rather than preparation for functional revival. This
 *   constraint is an exemplar of how commitment systems sustain themselves
 *   through textual authority when performance becomes impossible: the kernel
 *   (the Torah's commands concerning sacrifice) becomes stabilized through
 *   interpretive mediation (the Talmudic tradition of study), which in turn
 *   becomes an institutional apparatus that extracts authority from the
 *   deferral itself.
 *
 * KEY AGENTS:
 *   - Rabbinic Interpretive Authority: Primary beneficiary (institutional/arbitrage) — generates scholarly legitimacy, interpretive prestige, and institutional power through control of the textual apparatus. Can shift readings at will through reinterpretation.
 *   - Kohen Lineage: Primary victim (powerless/trapped) — bears identity-locked trap status; maintains hereditary priestly privilege with no functional exercise. Exit is structurally and identity-wise impossible.
 *   - Alternative Ritual Communities (Karaite, Reform, Performance-Restoration): Secondary victims (moderate/constrained) — propose alternative readings (literalism, abandonment, restoration) but face institutional suppression and resource barriers. Coordinate around different solutions but are enforced toward rabbinic consensus.
 *   - Contemporary Textual Study Community: Mixed position (organized/constrained) — benefits from coordination function (intellectual community, shared meaning) but bears extraction costs through gatekeeping of interpretive innovation.
 *   - Textual Study Tradition (as institutional phenomenon): Institutional beneficiary (institutional/arbitrage) — sustained through enforcement apparatus and identity identification. Theater ratio indicates increasing performative content.
 *   - Analytical Observer: Universal/civilizational perspective (analytical/analytical) — risks naturalizing the constructed choice (study-as-exercise) as inevitable consequence of Temple destruction, thereby obscuring the institutional power consolidation.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(textual_authority_structure, 0.35).
domain_priors:suppression_score(textual_authority_structure, 0.45).
domain_priors:theater_ratio(textual_authority_structure, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(textual_authority_structure, extractiveness, 0.35).
narrative_ontology:constraint_metric(textual_authority_structure, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(textual_authority_structure, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(textual_authority_structure, tangled_rope).
narrative_ontology:human_readable(textual_authority_structure, "Textual Authority Structure in Rabbinic Temple Sacrifice Law").
narrative_ontology:topic_domain(textual_authority_structure, "religious_law/textual_tradition/ritual_studies").

domain_priors:requires_active_enforcement(textual_authority_structure).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(textual_authority_structure, '18e3e809-2210-4ddd-9269-b709cf7ab603').
narrative_ontology:cs_kernel_codification('18e3e809-2210-4ddd-9269-b709cf7ab603', fixed_text).
narrative_ontology:cs_authority_grounding('18e3e809-2210-4ddd-9269-b709cf7ab603', lineage).
narrative_ontology:cs_interpretation_layer_present('18e3e809-2210-4ddd-9269-b709cf7ab603').
narrative_ontology:cs_reading_relation('18e3e809-2210-4ddd-9269-b709cf7ab603', textual_authority_structure__karaite_performance_only_reading, coexists_with).
narrative_ontology:cs_reading_relation('18e3e809-2210-4ddd-9269-b709cf7ab603', textual_authority_structure__reform_contextual_reinterpretation, coexists_with).
narrative_ontology:cs_reading_relation('18e3e809-2210-4ddd-9269-b709cf7ab603', textual_authority_structure__restoration_messianic_deferral, influences).
narrative_ontology:cs_axiom('18e3e809-2210-4ddd-9269-b709cf7ab603', foundational, study_constitutes_performance).
narrative_ontology:cs_axiom_status(study_constitutes_performance, holdable).
narrative_ontology:cs_axiom_grounding('18e3e809-2210-4ddd-9269-b709cf7ab603', study_constitutes_performance, deontological).
narrative_ontology:cs_axiom('18e3e809-2210-4ddd-9269-b709cf7ab603', secondary, rabbinic_consensus_binds_interpretation).
narrative_ontology:cs_axiom_status(rabbinic_consensus_binds_interpretation, holdable).
narrative_ontology:cs_axiom_grounding('18e3e809-2210-4ddd-9269-b709cf7ab603', rabbinic_consensus_binds_interpretation, conventional).
narrative_ontology:cs_axiom('18e3e809-2210-4ddd-9269-b709cf7ab603', foundational, covenant_remains_live).
narrative_ontology:cs_axiom_status(covenant_remains_live, holdable).
narrative_ontology:cs_axiom_grounding('18e3e809-2210-4ddd-9269-b709cf7ab603', covenant_remains_live, deontological).
narrative_ontology:cs_reference_frame('18e3e809-2210-4ddd-9269-b709cf7ab603', textual_study_as_covenant_exercise).
narrative_ontology:cs_drift_state('18e3e809-2210-4ddd-9269-b709cf7ab603', contemporary_literary_criticism_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('18e3e809-2210-4ddd-9269-b709cf7ab603', '').

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(textual_authority_structure, rabbinic_interpretive_authority).
narrative_ontology:constraint_beneficiary(textual_authority_structure, textual_study_tradition).
narrative_ontology:constraint_victim(textual_authority_structure, performance_practitioners).
narrative_ontology:constraint_victim(textual_authority_structure, alternative_religious_modalities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(textual_authority_structure, contemporary_textual_study_community).
narrative_ontology:constraint_victim(textual_authority_structure, kohen_lineage).
narrative_ontology:constraint_victim(textual_authority_structure, karaite_literalist_community).
narrative_ontology:constraint_victim(textual_authority_structure, early_reform_judaism).
narrative_ontology:constraint_vindicates(textual_authority_structure, textual_authority_supersedes_performance).
narrative_ontology:constraint_vindicates(textual_authority_structure, continuous_study_maintains_covenant).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Controls and administers the textual apparatus through which Temple sacrifice law is preserved and interpreted. Produces scholarly consensus, enforces hermeneutic standards, and maintains institutional legitimacy through control of legitimate interpretation. Can shift the meaning of the law through reinterpretation at will (arbitrage exit: no cost to changing positions).
narrative_ontology:constraint_stakeholder(textual_authority_structure, rabbinic_interpretive_authority, agenda_setter,
    institutional, generational, arbitrage, global).

% Hereditary priestly lineage that retains formal status and obligation under Temple sacrifice law despite 1,900 years without functional exercise. Bears the cost of maintaining unfulfilled role. Identity-fused with priestly status; exit would require abandoning self-concept and group membership. The status confers symbolic privilege (certain honors and liturgical roles) but no operational outlet for the covenant commitment the law prescribes.
narrative_ontology:constraint_stakeholder(textual_authority_structure, kohen_lineage, payer,
    powerless, generational, identity_locked, global).

% Proposes alternative reading: that Temple sacrifice law requires actual performance or restoration rather than textual study. Constrained by institutional barriers (organizational power concentration in rabbinic networks), resource scarcity (smaller historical communities), and social pressure (deviation from consensus carries religious and social penalties). Also excluded from authoritative voice in mainstream interpretation — their alternative reading is marginalized despite textual legitimacy.
narrative_ontology:constraint_stakeholder(textual_authority_structure, karaite_literalist_community, payer,
    moderate, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(textual_authority_structure, karaite_literalist_community, excluded).

% Proposes abandonment of Temple sacrifice law as binding (contextual reinterpretation: the commands were context-specific, now superseded). Constrained by institutional barriers and also excluded from mainstream interpretation. The exclusion prevents their reading from entering the authoritative consensus, though they sustain their own interpretive communities.
narrative_ontology:constraint_stakeholder(textual_authority_structure, early_reform_judaism, payer,
    moderate, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(textual_authority_structure, early_reform_judaism, excluded).

% Contemporary practitioners (yeshiva students, academic scholars, online learning communities) who benefit from the study tradition: intellectual community, shared meaning-making, structured intellectual engagement. Constrained by gatekeeping (hierarchies of legitimate interpretation, academic institutions, rabbinic credentialing) and by enforcement of hermeneutic consensus. Can organize collectively but face institutional barriers to innovation or alternative readings.
narrative_ontology:constraint_stakeholder(textual_authority_structure, contemporary_textual_study_community, beneficiary,
    organized, biographical, constrained, global).

% The institutional structure of textual preservation and interpretation (yeshivas, academies, liturgical commemoration, theoretical jurisprudence). Not an agent but an institutional phenomenon that exhibits its own persistence logic. The apparatus sustains itself through performance and identity identification; the study obligation is institutionalized through religious law itself.
narrative_ontology:constraint_stakeholder(textual_authority_structure, textual_study_apparatus, agenda_setter,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_non_agent(textual_authority_structure, textual_study_apparatus).

% Literal Temple sacrifice performance (historically impossible for 1,900 years; reconstructionist efforts remain marginal). Excluded from mainstream discourse as an acceptable reading of the law. The exclusion is structural (Temple does not exist; political conditions do not permit reconstruction) and institutional (authoritative tradition does not recognize performance-restoration as a live option). Treated as a messianic deferral rather than a live legal obligation.
narrative_ontology:constraint_stakeholder(textual_authority_structure, temple_performance_alternative, excluded,
    powerless, civilizational, trapped, local).
narrative_ontology:stakeholder_non_agent(textual_authority_structure, temple_performance_alternative).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintain covenant fidelity and community identity in the absence of Temple performance. The textual apparatus provides a structured method for continuous engagement with the law's meaning, preserving the tradition's intellectual content, and sustaining the religious community's consciousness of obligation despite permanent structural impossibility of performance.
% TRANSFER_FUNCTION: Authority and interpretive legitimacy flow from the rabbinic interpretive authority to practitioners and communities. The authority system enforces a consensus reading (study-as-exercise) and suppresses alternatives (Karaite literalism, Reform abandonment, restoration movements). Status flows to rabbinic scholars and institutional gatekeepers; constraints and obligations flow to practitioners, communities, and the powerless Kohen lineage with no functional outlet.
% ABSENT_VOICES: Temple priests (historically deceased) and their functional successors (Kohanim seeking to actually perform the sacrifice); performance-restoration movements (marginal, excluded from mainstream discourse); contemporary reconstructionist Judaism (excluded from normative authority); alternative textual communities (Samaritans, medieval sectarian groups, modern literalist movements) whose readings are not represented in the authoritative consensus apparatus.
% DISAPPEARANCE_RATIONALE: If the textual authority structure disappeared (if the study apparatus ceased to function and the rabbinic consensus dissolved), the religious community would require alternative mechanisms for covenant maintenance, identity preservation, and intellectual engagement with the tradition. The constraint is not a natural law — it is a constructed institutional arrangement. Alternative arrangements exist and have existed (Karaite literalism, Reform theology, secular Jewish culture). The disappearance would force explicit choice among these alternatives rather than their implicit suppression under rabbinic consensus.
% FOUNDING_PROBLEM: The destruction of the Second Temple in 70 CE rendered literal performance of Temple sacrifice law impossible while the Torah retained commanding force. The founding problem was not merely historical contingency but theological crisis: how to maintain covenant fidelity when the primary means of fulfilling the law became structurally impossible.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (Temple destruction making sacrifice impossible) is attested by historical record, archaeological evidence, and universal agreement across all Jewish interpretive traditions — Rabbinic, Karaite, Reform, Orthodox, secular. Even those who debate the theological or legal meaning of the situation (omega_1: study-as-exercise vs archival) agree on the historical fact (Temple is gone, performance is impossible). The agreement on problem-status but disagreement on response is the core of the constraint.
narrative_ontology:disappearance_verdict(textual_authority_structure, world_rearranges).
narrative_ontology:founding_problem_status(textual_authority_structure, dead).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: KOHEN LINEAGE (SNARE) — Trapped in a hereditary status that confers no functional exercise. Temple sacrifice law is studied but never performed; the priestly privilege that law once instantiated has no operational outlet. Exit is impossible (lineage status is constitutive identity); the trap is complete. Extraction: the study apparatus claims to occupy the priest's mandate while performance remains deferred indefinitely. The lineage bears the cost of maintaining an unfulfilled role.
constraint_indexing:constraint_classification(textual_authority_structure, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: ALTERNATIVE RITUAL COMMUNITY (TANGLED ROPE) — Constrained by institutional pressure and resource barriers, but also derives benefit from the rabbinic textual system when it serves their own coordination needs. They coordinate around alternative readings of the law (performance-only vs study-as-exercise) but their alternatives are suppressed through institutional enforcement of the rabbinic consensus. Genuine coordination function (how to preserve the covenant without Temple) paired with asymmetric extraction (enforcement of one reading over others).
constraint_indexing:constraint_classification(textual_authority_structure, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: RABBINIC INTERPRETIVE AUTHORITY (ROPE) — Institutional beneficiary with arbitrage options (can shift the reading at will through reinterpretation). Experiences the constraint as coordination: the textual system enables continuous interpretation and authority production. The apparatus generates scholarly careers, institutional legitimacy, and interpretive prestige. Net beneficiary — extraction runs toward this institution, not from it. Exit is costless (institutional agents can always reframe the interpretation).
constraint_indexing:constraint_classification(textual_authority_structure, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: PERFORMANCE-DEFERRAL RITUAL (PITON) — The study practice maintains a ritualistic form (liturgical commemoration of sacrifices, theoretical discussion of law) that is substantially theatrical. The functional justification (preparing for restoration of Temple performance) has atrophied as historical contingency (Temple destruction) has become permanent institutional fact. The ritual persists through liturgical inertia and identity tradition rather than because its stated function (readying for future performance) is live. The constraint's theater_ratio (0.68) reflects this degradation: most of the performance is symbolic maintenance, not preparation.
constraint_indexing:constraint_classification(textual_authority_structure, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER / TEXTUAL AUTHORITY NATURALIZATION (MOUNTAIN) — From a civilizational, universal view, the constraint appears as an immutable feature of how textual traditions preserve commitments: study without performance is the only possible mode once performance becomes structurally impossible. This perspective naturalizes the choice between study-as-exercise and archival-preservation as inevitable. However, this is a FALSE SUMMIT — the naturalization conceals the constructed institutional choice: the rabbinic authority CHOSE the study-as-exercise reading to maintain its own interpretive legitimacy, and this choice is enforced despite available alternatives (Karaite, Reform, performance-deferral readings).
constraint_indexing:constraint_classification(textual_authority_structure, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 6: TEXTUAL STUDY COMMUNITY (TANGLED ROPE) — Organized agents (yeshiva networks, academic institutions, online learning communities) benefit from the coordination function of the study tradition: it provides structure, meaning, and community. But they also bear extraction costs — the system privileges interpretive consensus over textual innovation, and alternative readings are suppressed through institutional gatekeeping. Genuine coordination (shared study, intellectual community) paired with asymmetric extraction (enforcement of rabbinic hermeneutics).
constraint_indexing:constraint_classification(textual_authority_structure, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(textual_authority_structure_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(textual_authority_structure, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(textual_authority_structure, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(textual_authority_structure, TR),
    TR >= 0.70.

:- end_tests(textual_authority_structure_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.35): Moderate. The constraint exhibits genuine coordination function (studying the law preserves the tradition, maintains covenant consciousness, and provides intellectual community). However, the extraction component is also real: the rabbinic interpretive authority enforces the study-as-exercise reading to maintain institutional legitimacy and suppress alternatives. The value reflects that both functions are present and roughly balanced — neither coordination nor extraction dominates entirely. Suppression (0.45): Moderate. Alternative readings face institutional barriers (gatekeeping through yeshiva networks, academic hierarchies, rabbinic consensus enforcement) but are not completely eliminated (Karaite and Reform traditions persist). The suppression is sustained through institutional authority rather than physical coercion, which makes it permeable but durable. Theater ratio (0.68): Elevated and rising. The ritual apparatus increasingly appears as symbolic maintenance rather than preparation for functional revival: liturgical commemoration of sacrifices follows the format of actual Temple service but cannot fulfill its stated function. As centuries pass and Temple restoration becomes less plausible, the theater ratio rises. At t=0 (immediately post-destruction), the apparatus could be interpreted as active preparation; by t=1900, it is clearly maintenance of symbolic identity. The measurement trajectory (0.35→0.68 over 1,900 years) shows degradation consistent with the Piton perspective.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates the perspectival multiplicity of commitment-system constraints. The Rabbinic Interpretive Authority sees Rope: they are solving the genuine problem of maintaining covenant fidelity when Temple performance is impossible; the study apparatus enables continuous interpretation and authority production. The Kohen Lineage sees Snare: trapped in a status with no outlet; the study apparatus claims to occupy their mandate while performance remains deferred indefinitely. The Alternative Communities see Tangled Rope: genuine coordination problems (how to preserve the tradition?) mixed with asymmetric extraction (rabbinic enforced consensus suppresses their readings). The Analytical Observer sees Mountain: the study-without-performance structure appears as immutable consequence of Temple destruction — but this is false-summit naturalization. The constructed choice (study-as-exercise vs archival preservation) is presented as inevitable. The Contemporary Study Community sees Tangled Rope: benefiting from intellectual community but bearing costs of interpretive gatekeeping. The Performance-Deferral Ritual sees Piton: the apparatus is increasingly performative rather than functional, maintained through inertia.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective derives from structural position: beneficiary status, victim status, power level, and exit options. The Rabbinic Authority (institutional/arbitrage) derives d from beneficiary status + institutional power + arbitrage exit → low d → negative effective extraction (they collect from the constraint). The Kohen Lineage (powerless/trapped) derives d from victim status + powerless power + trapped exit → high d → high effective extraction (the constraint extracts from them). The Alternative Communities (moderate/constrained) derive d from mixed victim/beneficiary status + moderate power + constrained exit → moderate d → moderate extraction (genuine coordination mixed with suppression). The study Community (organized/constrained) derives d from beneficiary status mixed with suppression cost + organized power + constrained exit → moderate-low d → low-to-moderate extraction. The Analytical Observer (analytical/analytical) derives d from no beneficiary/victim status + analytical power + analytical exit → d = 0.5 (symmetric observation point). The directionality overrides are not needed; the structural derivation produces accurate results.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy question: Does the textual authority structure serve its founding purpose (maintaining covenant fidelity in the absence of Temple performance), or has the apparatus become an end in itself? UNRESOLVED. The constraint shows classical symptoms of mandatrophy: (1) the mandate (preserve the covenant through study-as-exercise) is contested (omega_1: study-as-exercise vs archival); (2) the apparatus exhibits increasing theater ratio (degradation toward performative maintenance); (3) the original problem (Temple destruction) is permanent institutional fact, not a temporary condition awaiting restoration. However, the constraint also shows genuine ongoing coordination function: the study tradition creates real intellectual community and meaning for practitioners. The mandatrophy is irresolvable without first resolving omega_1 (study-as-exercise vs archival). If study-as-exercise is genuine, then the founding purpose is live (not mandatrophy). If study is archival cover story, then mandatrophy is acute — the apparatus maintains itself through performance rather than function. This ambiguity is the defining feature of the constraint.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    study_as_exercise_vs_archival,
    'Does continuous textual study of Temple sacrifice law constitute genuine exercise of the priestly/Levitical covenant commitment, or is it archival preservation of a defunct practice?',
    'Textual-historical analysis of rabbinic intent in preserving Kodashim through two millennia; examination of whether the study-as-exercise framing appears in authoritative sources or is post-hoc rationalization; comparative analysis with other suspended-practice traditions (e.g., Sabbatical year law, Temple pilgrimage obligation).',
    'If study-as-exercise is genuine (study counts as performance): constraint is primarily coordination (Rope from more perspectives). If study is archival cover story: constraint is primarily extraction (Snare from more perspectives). Classification shifts between Rope-dominant and Snare-dominant depend on resolution.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(study_as_exercise_vs_archival, empirical, 'Whether study without performance constitutes genuine covenant exercise or archival preservation').

omega_variable(
    natural_law_vs_constructed_choice,
    'Is the study-without-performance structure an immutable consequence of Temple destruction (natural law), or a constructed institutional choice that suppresses available alternatives?',
    'Historical analysis of whether alternative responses existed and were considered (Karaite literalism, early Reform abandonment, Messianic deferral, contemporary Reconstructionist reinterpretation); examination of whether the rabbinic choice was inevitable or contingent.',
    'If immutable: mountain classification is accurate. If contingent: false summit detection applies — beneficiary institutions naturalize their choice as inevitable. Classification shifts from mountain to tangled_rope or snare in perspective that recognizes contingency.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_choice, conceptual, 'Whether study-without-performance is natural necessity or constructed institutional choice').

omega_variable(
    suppression_mechanism_legitimacy,
    'What justifies the institutional suppression of alternative readings (Karaite, Reform, performance-restoration movements) within the rabbinic tradition?',
    'Examination of textual authority claims used to enforce consensus; historical analysis of how alternatives were marginalized; assessment of whether suppression rests on empirical hermeneutic dispute or institutional power consolidation.',
    'If suppression is justified by superior textual interpretation: constraint is legitimate tangled rope (mixed coordination and principled extraction). If suppression is primarily institutional gatekeeping: constraint becomes snare (pure extraction masked as coordination). Directionality implications for victims/beneficiaries.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_legitimacy, empirical, 'Legitimacy basis for suppression of alternative readings').

omega_variable(
    identity_locked_priestly_status,
    'Is the Kohen lineage''s trap status structural (no performance opportunity exists) or identity-locked (the priestly identity makes exit from the covenant unthinkable)?',
    'Analysis of whether Kohanim could exit the tradition entirely (structural mobility); examination of whether identity-as-priest is constitutive of self-concept (cognitive entrapment); historical cases of Kohanim who abandoned the tradition and whether they experienced this as abandoning identity.',
    'If trapped (structural barriers only): classification as Snare is accurate. If identity_locked (cognitive + structural): the trap is maintained through internal identification with the covenant; exit costs are higher than structural barriers alone suggest; the constraint''s psychological cost is underestimated.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(identity_locked_priestly_status, empirical, 'Whether Kohen lineage trap is structural or identity-locked').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(textual_authority_structure, 0, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tas_tr_t0, textual_authority_structure, theater_ratio, 0, 0.35).
narrative_ontology:measurement(tas_tr_t500, textual_authority_structure, theater_ratio, 500, 0.5).
narrative_ontology:measurement(tas_tr_t1000, textual_authority_structure, theater_ratio, 1000, 0.65).
narrative_ontology:measurement(tas_tr_t1900, textual_authority_structure, theater_ratio, 1900, 0.68).

% Extraction over time
narrative_ontology:measurement(tas_be_t0, textual_authority_structure, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(tas_be_t500, textual_authority_structure, base_extractiveness, 500, 0.32).
narrative_ontology:measurement(tas_be_t1000, textual_authority_structure, base_extractiveness, 1000, 0.34).
narrative_ontology:measurement(tas_be_t1900, textual_authority_structure, base_extractiveness, 1900, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(tas_su_t0, textual_authority_structure, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(tas_su_t500, textual_authority_structure, suppression_requirement, 500, 0.4).
narrative_ontology:measurement(tas_su_t1000, textual_authority_structure, suppression_requirement, 1000, 0.45).
narrative_ontology:measurement(tas_su_t1900, textual_authority_structure, suppression_requirement, 1900, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(textual_authority_structure, identity_coordination).
narrative_ontology:affects_constraint(textual_authority_structure, karaite_literalism_suppression).
narrative_ontology:affects_constraint(textual_authority_structure, reform_judaism_interpretive_authority).
narrative_ontology:affects_constraint(textual_authority_structure, temple_restoration_deferral_mechanism).

% DUAL FORMULATION NOTE:
% The textual authority structure decomposes into three narratively linked constraints with different ε values: (1) Study-as-exercise reading (this story, moderate ε): genuine coordination + institutional enforcement. (2) Karaite literalism suppression (downstream, high ε): pure extraction through institutional gatekeeping. (3) Temple restoration deferral (upstream, low ε): coordination around covenant maintenance. Each story has its own beneficiary/victim structure and classification. The network declares the genealogical relationships.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(textual_authority_structure, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
