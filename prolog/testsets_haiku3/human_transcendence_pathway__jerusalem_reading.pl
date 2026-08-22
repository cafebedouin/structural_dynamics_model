% ============================================================================
% CONSTRAINT STORY: human_transcendence_pathway__jerusalem_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_human_transcendence_pathway__jerusalem_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: human_transcendence_pathway__jerusalem_reading
 *   human_readable: Participatory Communion Under Divine Blessing (Jerusalem Reading)
 *   domain: religious/political_theology/social
 *
 * SUMMARY:
 *   The Jerusalem reading of authentic human transcendence envisions
 *   communities rebuilt through patient, participatory labor that integrates
 *   diversity as a resource for communion rather than a problem requiring
 *   elimination. Unlike the Babel reading (unified technological systems
 *   achieving stability without transcendent reference) or the technocratic
 *   reading (transcendence through optimization and elimination of human
 *   limits), the Jerusalem reading grounds human flourishing in divine
 *   blessing and incarnational gift. Beneficiaries are the faith community as
 *   a whole and especially those returning from exile or marginalization. The
 *   constraint operates through formation, catechesis, and lived practice
 *   rather than coercion — suppression is minimal because the constraint's
 *   appeal is primarily motivational (the attractiveness of genuine
 *   belonging) rather than enforced. The measurement series shows
 *   extractiveness rising modestly from early formation (0.18) to mature
 *   practice (0.28–0.29) as the constraint deepens, plateauing as communities
 *   achieve stable communion. Theater ratio remains very low (0.05–0.09),
 *   indicating strong functional alignment: communities enacting this
 *   constraint spend minimal effort on performative maintenance — the labor
 *   is genuine.
 *
 * KEY AGENTS:
 *   - faith_community_practitioners: Organized communities that enact the constraint through formation, deliberation, and participatory labor; set the rules through lived practice
 *   - returning_exiles_and_marginalized: Structurally beneficiary; their reintegration with voice and agency is the measure of authentic communion
 *   - technological_efficiency_advocates: Excluded by the constraint's own logic; the Jerusalem reading rejects instrumental transcendence
 *   - secular_liberal_pluralism_advocates: Excluded because they bracket the theological grounding the reading depends on
 *   - church_magisterium: Institutional agenda-setter that articulates and teaches the constraint; maintains it through formation
 *   - divine_authority_grounding: Non-agent structural element — the transcendent source of the constraint's legitimacy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(human_transcendence_pathway__jerusalem_reading, 0.28).
domain_priors:suppression_score(human_transcendence_pathway__jerusalem_reading, 0.12).
domain_priors:theater_ratio(human_transcendence_pathway__jerusalem_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(human_transcendence_pathway__jerusalem_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(human_transcendence_pathway__jerusalem_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(human_transcendence_pathway__jerusalem_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(human_transcendence_pathway__jerusalem_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(human_transcendence_pathway__jerusalem_reading, resistance, 0.22).

% --- Constraint claim ---
narrative_ontology:constraint_claim(human_transcendence_pathway__jerusalem_reading, rope).
narrative_ontology:human_readable(human_transcendence_pathway__jerusalem_reading, "Participatory Communion Under Divine Blessing (Jerusalem Reading)").
narrative_ontology:topic_domain(human_transcendence_pathway__jerusalem_reading, "religious/political_theology/social").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(human_transcendence_pathway__jerusalem_reading, '8cb52fdb-0776-44c1-9ac2-efd5f2e84150').
narrative_ontology:cs_kernel_codification('8cb52fdb-0776-44c1-9ac2-efd5f2e84150', fixed_text).
narrative_ontology:cs_authority_grounding('8cb52fdb-0776-44c1-9ac2-efd5f2e84150', lineage).
narrative_ontology:cs_interpretation_layer_present('8cb52fdb-0776-44c1-9ac2-efd5f2e84150').
narrative_ontology:cs_reading_relation('8cb52fdb-0776-44c1-9ac2-efd5f2e84150', human_transcendence_pathway__babel_reading, forecloses).
narrative_ontology:cs_reading_relation('8cb52fdb-0776-44c1-9ac2-efd5f2e84150', human_transcendence_pathway__technocratic_vs_incarnational_reading, coexists_with).
narrative_ontology:cs_axiom('8cb52fdb-0776-44c1-9ac2-efd5f2e84150', foundational, transcendence_as_divine_gift_in_vulnerability).
narrative_ontology:cs_axiom_status(transcendence_as_divine_gift_in_vulnerability, holdable).
narrative_ontology:cs_axiom_grounding('8cb52fdb-0776-44c1-9ac2-efd5f2e84150', transcendence_as_divine_gift_in_vulnerability, deontological).
narrative_ontology:cs_axiom('8cb52fdb-0776-44c1-9ac2-efd5f2e84150', foundational, authentic_diversity_as_communion_resource).
narrative_ontology:cs_axiom_status(authentic_diversity_as_communion_resource, holdable).
narrative_ontology:cs_axiom_grounding('8cb52fdb-0776-44c1-9ac2-efd5f2e84150', authentic_diversity_as_communion_resource, conventional).
narrative_ontology:cs_axiom('8cb52fdb-0776-44c1-9ac2-efd5f2e84150', secondary, participatory_labor_necessary_not_technical_substitution).
narrative_ontology:cs_axiom_status(participatory_labor_necessary_not_technical_substitution, holdable).
narrative_ontology:cs_axiom_grounding('8cb52fdb-0776-44c1-9ac2-efd5f2e84150', participatory_labor_necessary_not_technical_substitution, instrumental).
narrative_ontology:cs_reference_frame('8cb52fdb-0776-44c1-9ac2-efd5f2e84150', incarnational_communion_framework).
narrative_ontology:cs_drift_state('8cb52fdb-0776-44c1-9ac2-efd5f2e84150', contemporary_technological_saturation, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('8cb52fdb-0776-44c1-9ac2-efd5f2e84150', '').
narrative_ontology:cs_kernel_id(human_transcendence_pathway__jerusalem_reading, human_transcendence_pathway).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(human_transcendence_pathway__jerusalem_reading, returning_exiles_and_marginalized).
narrative_ontology:constraint_beneficiary(human_transcendence_pathway__jerusalem_reading, faith_community_as_whole).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(human_transcendence_pathway__jerusalem_reading, faith_community_practitioners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Organized practitioners of participatory community-building who set the constraint through lived practice, catechesis, and formation. They enact the slow labor of integration, deliberately preserve plural voices (linguistic, cultural, economic), and embody the constraint through patient dialogue. They collect the fruit of communion — genuine belonging — not as rent but as shared good. Their identity as members of the faith community is inseparable from this participation.
narrative_ontology:constraint_stakeholder(human_transcendence_pathway__jerusalem_reading, faith_community_practitioners, agenda_setter,
    organized, generational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(human_transcendence_pathway__jerusalem_reading, faith_community_practitioners, beneficiary).

% Those displaced, dispossessed, or structurally excluded find in this constraint a path of reintegration that honors their specific experience and voice rather than demanding conformity. They contribute their particular wisdom and labor; their inclusion is the measure of authentic communion. They bear the cost of slow rebuilding (efficiency foregone) but receive the deep good of belonging and agency.
narrative_ontology:constraint_stakeholder(human_transcendence_pathway__jerusalem_reading, returning_exiles_and_marginalized, beneficiary,
    powerless, generational, constrained, regional).

% Representatives of optimization frameworks, transhumanism, or unified rational systems that would substitute algorithmic/engineering solutions for participatory deliberation. They are excluded not by force but by the constraint's own logic: the Jerusalem reading rejects their premise that transcendence comes through elimination of limits and diversity. Their exclusion is structural, not enforced — the constraint simply does not admit their instrumental framing.
narrative_ontology:constraint_stakeholder(human_transcendence_pathway__jerusalem_reading, technological_efficiency_advocates, excluded,
    powerful, immediate, arbitrage, global).

% Proponents of neutral procedural frameworks that bracket transcendent claims to protect plural value systems equally. They would argue the constraint imposes a particular theological vision (divine blessing, incarnational anthropology) rather than remaining procedurally neutral. They object not to plurality itself but to grounding it in religious authority rather than secular institutional design.
narrative_ontology:constraint_stakeholder(human_transcendence_pathway__jerusalem_reading, secular_liberal_pluralism_advocates, excluded,
    organized, biographical, arbitrage, national).

% The transcendent source from which the constraint draws its legitimacy and binding force. Not an actor but a structural element: the constraint's persistence and appeal depend on belief that authentic human flourishing comes as gift from divine grace, not human construction. This is the axiom that distinguishes the Jerusalem reading from technocratic and Babel framings.
narrative_ontology:constraint_stakeholder(human_transcendence_pathway__jerusalem_reading, divine_authority_grounding, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(human_transcendence_pathway__jerusalem_reading, divine_authority_grounding).

% The teaching authority of the Catholic tradition that articulates and defends the Jerusalem reading against competing interpretations. Sets doctrine through conciliar processes, papal teaching, and theological discourse. Maintains the constraint through formation of practitioners and communities. Their institutional identity is bound to the propagation of this vision.
narrative_ontology:constraint_stakeholder(human_transcendence_pathway__jerusalem_reading, church_magisterium, agenda_setter,
    institutional, civilizational, trapped, global).

% External perspective analyzing the constraint without commitment to its success. Observes how the Jerusalem reading operates, where it succeeds and fails, and how it relates to competing readings.
narrative_ontology:constraint_stakeholder(human_transcendence_pathway__jerusalem_reading, analytical_observer, observer,
    analytical, biographical, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem of how human diversity can coexist as resource rather than threat, how marginalized voices can be genuinely integrated without erasure, and how communities can rebuild after exile/dispossession in ways that honor both the suffering and the agency of those who return. The coordination is achieved through patient formation and participatory labor grounded in divine blessing rather than procedural technique or optimization.
% TRANSFER_FUNCTION: Moves attention, labor, and material resources from efficiency maximization toward communion-building; individuals transfer their exclusive self-interest toward shared belonging and mutual responsibility. The constraint transfers authority from technocratic expertise toward wisdom grounded in lived experience and theological tradition. No extraction occurs — the transfers are reciprocal and non-coercive.
% ABSENT_VOICES: Technological systems designers and transhumanist advocates are structurally absent — the constraint's own logic excludes their framework. Secular proceduralists are absent because they reject the theological grounding the reading depends on. The voices of those disposed by prior efficiency systems who have not yet been invited into the community are genuinely missing — the constraint's implementation often fails to reach them.
% DISAPPEARANCE_RATIONALE: If this constraint vanished, communities would revert to either technocratic optimization (Babel reading) or procedural neutrality (secular liberal framework). The specific form of communion — where returning exiles find honored place, where plurality is preserved as resource, where divine blessing grounds legitimacy — would disappear. Existing faith communities would lose their theological moorings; individuals would reorganize around efficiency or procedural justice rather than incarnational belonging. The world would not be unchanged, but reorganized around different authority structures.
% FOUNDING_PROBLEM: Human communities face the choice between unity through imposed uniformity (Babel: the technological dream of seamless system) and fragmentation through pure procedural neutrality (secular liberalism: bracketing all substantive shared goods). The Jerusalem reading addresses a third path: authentic diversity held in communion through patient labor, grounded in divine grace rather than human construction. The founding problem is the false choice between uniformity and fragmentation.
% FOUNDING_PROBLEM_CORROBORATION: Catholic Social Doctrine teaching from Vatican II through Pope Francis attests the problem is live and urgent — see Evangelii Gaudium (ch. 4) on 'culture of encounter' and dignity of the marginalized, Fratelli Tutti on authentic brotherhood. Independent testimony from communities rebuilding after displacement (refugee resettlement programs, post-conflict reconciliation) confirms that neither pure efficiency systems nor neutral procedures adequately address the need for belonging-with-voice. The constraint is attested by practitioners who live it, not only by institutional authority.
narrative_ontology:disappearance_verdict(human_transcendence_pathway__jerusalem_reading, world_rearranges).
narrative_ontology:founding_problem_status(human_transcendence_pathway__jerusalem_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(human_transcendence_pathway__jerusalem_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(human_transcendence_pathway__jerusalem_reading, 'none', 1).
narrative_ontology:epsilon_provenance(human_transcendence_pathway__jerusalem_reading, 0.28, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(human_transcendence_pathway__jerusalem_reading_tests).
:- end_tests(human_transcendence_pathway__jerusalem_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low-to-moderate (0.28 at maturity) because the constraint operates primarily through persuasion, formation, and the attraction of genuine belonging rather than coercive extraction. The constraint does require something from participants — the sacrifice of efficiency for solidarity, the labor of patient dialogue, the vulnerability of genuine encounter — but this is experienced as gift, not extraction. The measurement trajectory is gentle and plateauing: extractiveness rises as the constraint matures (communities learn the practice) but stabilizes rather than spiraling (no Goodhart drift observed). Suppression is very low (0.12) because the constraint depends on voluntary participation grounded in theological conviction and community practice rather than external force. Theater ratio is minimal (0.08) because participatory practice requires genuine labor — the constraint cannot be maintained through performative gesture alone. Accessibility collapse is moderate (0.35) because alternatives remain available: individuals and communities can choose technocratic optimization or secular proceduralism, but once the Jerusalem reading is understood as offering genuine communion grounded in divine grace, its appeal is substantial. Resistance is low (0.22) because practitioners experience the constraint as liberating rather than oppressive — it frees them from the isolating logic of pure efficiency and procedural neutrality. The constraint is least resisted precisely where it is most mature and the experience of communion is deepest.
 *
 * PERSPECTIVAL GAP:
 *   Practitioners and marginalized beneficiaries experience the constraint as liberation and belonging; excluded efficiency advocates experience it as irrationality and regression; secular pluralists experience it as sectarian imposition. The engine computes these divergent seats from the structural data: practitioners have high identity-lock and organized power (moderate d toward beneficiary end), marginalized have powerlessness with constrained exit (high d but receiving genuine benefit, not extraction). Efficiency advocates have powerful arbitrage exit but are excluded by logic, not enforcement (their directionality is analytically ambiguous — they could enter but reject the premise). The constraint produces its most profound unity at the practitioner seats and its deepest resistance from those who remain excluded by choice.
 *
 * DIRECTIONALITY LOGIC:
 *   The faith community as beneficiary (practitioners + magisterium) sits near the beneficiary end: they set the rules, maintain the constraint through formation, and collect the fruit of communion. But this is NOT extraction — they collect shared good, not concentrated rent. Returning exiles and marginalized sit at the beneficiary end in a different way: they receive agency, voice, and belonging, though constrained by resource limits and requiring vulnerability. The constraint transfers nothing FROM these agents TO extraction beneficiaries; rather, it redirects resources (labor, attention, material support) toward communion-building. Efficiency advocates are excluded structurally (their premises are incompatible with the reading), not enforced out. The directionality profile is unusual for a 'rope' classification: all main stakeholders are beneficiaries or beneficiary-practitioners; there are no payers bearing extraction. The constraint persists because participants choose it, attesting to genuine coordination function.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (false choice between uniformity and fragmentation) remains live and urgent: contemporary debates about diversity/unity in civic life, immigration integration, and technological community design circle this exact problem. The constraint's teaching function (articulating the third path) remains vital; it does not show signs of atrophy or mandatrophy. However, implementation often lags teaching: faith communities frequently revert to efficiency logic or procedural neutrality in practice, even while affirming the Jerusalem reading in principle. This implementation gap — between the constraint as taught and as lived — is captured in the modest theater ratio (0.08): much of what appears as 'enacting communion' may be performative staging rather than genuine participatory labor. The constraint's resilience depends on whether communities can sustain the difficult practice of real dialogue and patient formation over time, or whether it becomes primarily a doctrinal position without embodied practice.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    formation_vs_performance_gap,
    'To what degree do faith communities actually enact participatory communion labor, versus maintaining the constraint as doctrinal performance or institutional theater?',
    'Ethnographic study of communities claiming the Jerusalem reading: measure ratio of deliberative time to procedural administration, track whether marginalized voices shape decisions or merely ratify them, compare resource allocation patterns over time.',
    'If formation is genuine (high ratio of deliberation to theater), the constraint is a working rope with authentic coordination function. If performance dominates, theater_ratio should rise and extractiveness with it; the constraint would degrade toward piton status.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(formation_vs_performance_gap, empirical, 'Whether the constraint operates as lived practice or institutional performance.').

omega_variable(
    divine_grounding_necessity,
    'Is divine blessing / incarnational anthropology structurally necessary for the Jerusalem reading''s communion function, or is it a contingent framing that could be maintained through secular theological substitute?',
    'Compare outcomes in communities that ground the constraint in explicit divine authority vs. those that attempt the same participatory practice grounded in secular humanism or procedural justice alone. Track divergence in resilience, depth of belonging, and maintenance effort.',
    'If divine grounding is constitutive (communities collapse or revert to efficiency without it), the constraint''s persistence depends on theological conviction and cannot be secularized without losing its distinctive form. If it is contingent, the constraint could be maintained as ''incarnational but secular'' practice — a different reading of the same kernel.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(divine_grounding_necessity, conceptual, 'Whether the constraint''s theological grounding is essential to its operation or contingent.').

omega_variable(
    inclusion_of_excluded_advocates,
    'Can practitioners of the Jerusalem reading genuinely include efficiency advocates and secular pluralists without compromising the constraint''s theological foundation, or does the constraint require structural exclusion of these readings?',
    'Track cases where communities attempt to hold all three readings simultaneously (Babel, Jerusalem, technocratic). Measure outcome: do they sustain plural coexistence, or does one reading eventually dominate?',
    'If genuine coexistence is possible, the constraint is more robust and less sectarian. If one reading must dominate, the Jerusalem reading''s claim to universal communion is limited to those who accept its premises — a cohesive but narrower community.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(inclusion_of_excluded_advocates, conceptual, 'Whether the constraint can accommodate its sibling readings without losing coherence.').

omega_variable(
    marginalized_agency_authenticity,
    'Do returning exiles and marginalized persons experience genuine agency and voice in the constraint''s practice, or does their inclusion depend on conformity to practitioner-set norms under the appearance of participation?',
    'Comparative study: measure decision influence (do marginalized voices change outcomes or merely participate), track resource flows (does material support follow marginalized priorities or practitioner judgment), record exit rates (do marginalized remain when their agency is threatened).',
    'If agency is genuine, the constraint delivers its promised benefit and deserves its rope classification. If inclusion is conditional on conformity, the constraint conceals an asymmetric extraction from marginalized voices — it would reclassify toward snare or tangled_rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(marginalized_agency_authenticity, empirical, 'Whether marginalized beneficiaries have authentic agency or conditional inclusion.').

omega_variable(
    kernel_reading_foreclosure_structure,
    'Do the three readings of the human_transcendence_pathway kernel logically foreclose one another, or do they coexist as genuinely live alternatives?',
    'Logical analysis: identify core axioms of each reading and test for logical contradiction. Empirical analysis: track practitioners who hold multiple readings simultaneously or shift between them; measure whether shifts involve contradiction or reframing.',
    'If readings foreclose (one logically rules out the others), classification of the kernel as irreconcilable; the Jerusalem reading''s claim to universal communion is bounded. If readings coexist, the kernel is a genuine site of pluralism and the Jerusalem reading must integrate its competitors'' insights to maintain coherence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure_structure, conceptual, 'Logical structure of sibling readings: do they foreclose or coexist?').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(human_transcendence_pathway__jerusalem_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(huma_tr_t0, human_transcendence_pathway__jerusalem_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement_basis(huma_tr_t0, observed).
narrative_ontology:measurement(huma_tr_t5, human_transcendence_pathway__jerusalem_reading, theater_ratio, 5, 0.06).
narrative_ontology:measurement_basis(huma_tr_t5, observed).
narrative_ontology:measurement(huma_tr_t10, human_transcendence_pathway__jerusalem_reading, theater_ratio, 10, 0.07).
narrative_ontology:measurement_basis(huma_tr_t10, observed).
narrative_ontology:measurement(huma_tr_t20, human_transcendence_pathway__jerusalem_reading, theater_ratio, 20, 0.08).
narrative_ontology:measurement_basis(huma_tr_t20, observed).
narrative_ontology:measurement(huma_tr_t30, human_transcendence_pathway__jerusalem_reading, theater_ratio, 30, 0.09).
narrative_ontology:measurement_basis(huma_tr_t30, observed).
narrative_ontology:measurement(huma_tr_t40, human_transcendence_pathway__jerusalem_reading, theater_ratio, 40, 0.08).
narrative_ontology:measurement_basis(huma_tr_t40, projected).

% Extraction over time
narrative_ontology:measurement(huma_be_t0, human_transcendence_pathway__jerusalem_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement_basis(huma_be_t0, observed).
narrative_ontology:measurement(huma_be_t5, human_transcendence_pathway__jerusalem_reading, base_extractiveness, 5, 0.22).
narrative_ontology:measurement_basis(huma_be_t5, observed).
narrative_ontology:measurement(huma_be_t10, human_transcendence_pathway__jerusalem_reading, base_extractiveness, 10, 0.25).
narrative_ontology:measurement_basis(huma_be_t10, observed).
narrative_ontology:measurement(huma_be_t20, human_transcendence_pathway__jerusalem_reading, base_extractiveness, 20, 0.28).
narrative_ontology:measurement_basis(huma_be_t20, observed).
narrative_ontology:measurement(huma_be_t30, human_transcendence_pathway__jerusalem_reading, base_extractiveness, 30, 0.29).
narrative_ontology:measurement_basis(huma_be_t30, observed).
narrative_ontology:measurement(huma_be_t40, human_transcendence_pathway__jerusalem_reading, base_extractiveness, 40, 0.28).
narrative_ontology:measurement_basis(huma_be_t40, projected).

% Suppression requirement over time
narrative_ontology:measurement(huma_su_t0, human_transcendence_pathway__jerusalem_reading, suppression_requirement, 0, 0.08).
narrative_ontology:measurement_basis(huma_su_t0, observed).
narrative_ontology:measurement(huma_su_t5, human_transcendence_pathway__jerusalem_reading, suppression_requirement, 5, 0.09).
narrative_ontology:measurement_basis(huma_su_t5, observed).
narrative_ontology:measurement(huma_su_t10, human_transcendence_pathway__jerusalem_reading, suppression_requirement, 10, 0.11).
narrative_ontology:measurement_basis(huma_su_t10, observed).
narrative_ontology:measurement(huma_su_t20, human_transcendence_pathway__jerusalem_reading, suppression_requirement, 20, 0.12).
narrative_ontology:measurement_basis(huma_su_t20, observed).
narrative_ontology:measurement(huma_su_t30, human_transcendence_pathway__jerusalem_reading, suppression_requirement, 30, 0.13).
narrative_ontology:measurement_basis(huma_su_t30, observed).
narrative_ontology:measurement(huma_su_t40, human_transcendence_pathway__jerusalem_reading, suppression_requirement, 40, 0.12).
narrative_ontology:measurement_basis(huma_su_t40, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(human_transcendence_pathway__jerusalem_reading, attachment_coordination).
narrative_ontology:boltzmann_floor_override(human_transcendence_pathway__jerusalem_reading, 0.12).
narrative_ontology:affects_constraint(human_transcendence_pathway__jerusalem_reading, human_transcendence_pathway__babel_reading).
narrative_ontology:affects_constraint(human_transcendence_pathway__jerusalem_reading, human_transcendence_pathway__technocratic_vs_incarnational_reading).

% DUAL FORMULATION NOTE:
% The Jerusalem reading is one of three readings of the human_transcendence_pathway kernel. The Babel reading (constraint_id: human_transcendence_pathway__babel_reading) proposes transcendence through unified technological systems. The technocratic vs. incarnational reading (constraint_id: human_transcendence_pathway__technocratic_vs_incarnational_reading) proposes a binary: either transcendence through technological optimization or transcendence as divine gift in vulnerability. The Jerusalem reading rejects both the Babel premise (that technology can substitute for divine grounding) and the binary structure of the technocratic reading. All three readings share a kernel (how human transcendence is achieved and grounded), but each answers differently. Network links establish that the readings are related; the detailed epistemic relationships are recorded in cs_structure.reading_relations and axioms below.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
