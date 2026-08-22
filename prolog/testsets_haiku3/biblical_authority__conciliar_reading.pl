% ============================================================================
% CONSTRAINT STORY: biblical_authority__conciliar_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_biblical_authority__conciliar_reading, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: biblical_authority__conciliar_reading
 *   human_readable: Biblical Authority via Conciliar and Patristic Consensus
 *   domain: theology/religious_history
 *
 * SUMMARY:
 *   This constraint instantiates the conciliar reading of biblical authority:
 *   Scripture is interpreted authoritatively through the consensus of
 *   ecumenical councils and the patristic tradition, understood as a living
 *   continuity rather than a static deposit or papal decree. The reading
 *   emerges historically in Eastern Christianity and is recognized by
 *   Orthodox and Oriental Orthodox communions. The constraint operates as a
 *   tangled rope: it coordinates doctrine across dispersed bishoprics
 *   (genuine coordination function) while extracting hermeneutical authority
 *   from individual interpreters and rapid adaptation (asymmetric
 *   extraction). The structural delta from sibling readings is moderate
 *   clerical extraction (episcopal, not papal), moderate fragmentation
 *   (autocephalous churches maintain local variation within conciliar
 *   bounds), sacraments as mysteries transcending rational systematization,
 *   and episcopal collegiality as the beneficiary. The founding problem
 *   (doctrinal fragmentation) is contested: some see it as live and unsolved
 *   without councils; others see it as solved by historical scholarship and
 *   ecumenical dialogue; still others see the councils as necessary but
 *   insufficient without a living magisterium.
 *
 * KEY AGENTS:
 *   - episcopal_collegiality: Organized seat that sets hermeneutical standards and convenes councils; benefits from collegial authority grounding against papacy
 *   - rapid_doctrinal_adaptation: Constrained payer; must show retrospective patristic coherence before innovation is recognized
 *   - individual_scriptural_interpretation: Powerless payer; identity-locked into accepting conciliar consensus or rupturing communion
 *   - ecumenical_councils: Institutional agenda-setter; enforces conciliar binding via council declarations
 *   - sola_scriptura_reformers: Excluded seat; would argue councils are fallible and Scripture self-interpreting
 *   - magisterial_centralization_advocates: Excluded seat; would argue doctrine must develop via living papal authority
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(biblical_authority__conciliar_reading, 0.48).
domain_priors:suppression_score(biblical_authority__conciliar_reading, 0.52).
domain_priors:theater_ratio(biblical_authority__conciliar_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(biblical_authority__conciliar_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(biblical_authority__conciliar_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(biblical_authority__conciliar_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(biblical_authority__conciliar_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(biblical_authority__conciliar_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(biblical_authority__conciliar_reading, tangled_rope).
narrative_ontology:human_readable(biblical_authority__conciliar_reading, "Biblical Authority via Conciliar and Patristic Consensus").
narrative_ontology:topic_domain(biblical_authority__conciliar_reading, "theology/religious_history").

domain_priors:requires_active_enforcement(biblical_authority__conciliar_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(biblical_authority__conciliar_reading, 'ec8e427b-db83-4c0b-909e-5412411f705e').
narrative_ontology:cs_kernel_codification('ec8e427b-db83-4c0b-909e-5412411f705e', fixed_text).
narrative_ontology:cs_authority_grounding('ec8e427b-db83-4c0b-909e-5412411f705e', lineage).
narrative_ontology:cs_interpretation_layer_present('ec8e427b-db83-4c0b-909e-5412411f705e').
narrative_ontology:cs_reading_relation('ec8e427b-db83-4c0b-909e-5412411f705e', biblical_authority__sola_scriptura_reading, forecloses).
narrative_ontology:cs_reading_relation('ec8e427b-db83-4c0b-909e-5412411f705e', biblical_authority__tradition_scripture_reading, coexists_with).
narrative_ontology:cs_axiom('ec8e427b-db83-4c0b-909e-5412411f705e', foundational, councils_authoritative_interpreter).
narrative_ontology:cs_axiom_status(councils_authoritative_interpreter, holdable).
narrative_ontology:cs_axiom_grounding('ec8e427b-db83-4c0b-909e-5412411f705e', councils_authoritative_interpreter, conventional).
narrative_ontology:cs_axiom('ec8e427b-db83-4c0b-909e-5412411f705e', foundational, patristic_consensus_binding).
narrative_ontology:cs_axiom_status(patristic_consensus_binding, holdable).
narrative_ontology:cs_axiom_grounding('ec8e427b-db83-4c0b-909e-5412411f705e', patristic_consensus_binding, deontological).
narrative_ontology:cs_reference_frame('ec8e427b-db83-4c0b-909e-5412411f705e', apostolic_conciliar_consensus).
narrative_ontology:cs_drift_state('ec8e427b-db83-4c0b-909e-5412411f705e', contemporary_pluralistic_context, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('ec8e427b-db83-4c0b-909e-5412411f705e', '').
narrative_ontology:cs_kernel_id(biblical_authority__conciliar_reading, biblical_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(biblical_authority__conciliar_reading, episcopal_collegiality).
narrative_ontology:constraint_victim(biblical_authority__conciliar_reading, rapid_doctrinal_adaptation).
narrative_ontology:constraint_victim(biblical_authority__conciliar_reading, individual_scriptural_interpretation).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(biblical_authority__conciliar_reading, historical_continuity_communities).
narrative_ontology:constraint_vindicates(biblical_authority__conciliar_reading, apostolic_succession_doctrine).
narrative_ontology:constraint_vindicates(biblical_authority__conciliar_reading, ecumenical_council_infallibility_claim).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% A network of bishops organized into autocephalous or communion structures who collectively interpret Scripture through councils and patristic consensus. They set the hermeneutical standard, convene councils to settle doctrinal disputes, and maintain the living tradition as the authoritative reading apparatus. They benefit from having their interpretive authority legitimated by appeal to ancient consensus rather than papal decree, preserving their collegial role against centralized magisterium. Their exit from this arrangement is rupture with conciliar authority and loss of recognition as legitimate bishops.
narrative_ontology:constraint_stakeholder(biblical_authority__conciliar_reading, episcopal_collegiality, agenda_setter,
    organized, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(biblical_authority__conciliar_reading, episcopal_collegiality, beneficiary).

% Reformers, theologians, and communities seeking to adapt doctrine to new historical circumstances, scientific discoveries, or pastoral contexts. They are constrained by the requirement that any new interpretation must be shown to cohere with patristic consensus and council declarations. Innovation is filtered through retrospective legitimation rather than prospective development. They cannot simply declare a doctrine obsolete or reinterpret it radically; it must be shown to have been 'always held' in the patristic tradition. Exit means anathema or schism, which destroys their identity within the Orthodox communion.
narrative_ontology:constraint_stakeholder(biblical_authority__conciliar_reading, rapid_doctrinal_adaptation, payer,
    moderate, biographical, identity_locked, continental).

% Individual believers, lay theologians, and local communities who read Scripture and derive theological conclusions. They are constrained by the requirement to align their readings with the authoritative conciliar and patristic consensus. Direct Scripture reading is permitted but must defer to the collegial interpretive standard. Their exit (refusing to defer to consensus) means rupture from communion and loss of sacramental participation. Their identity as Orthodox believers is constituted through acceptance of conciliar authority.
narrative_ontology:constraint_stakeholder(biblical_authority__conciliar_reading, individual_scriptural_interpretation, payer,
    powerless, biographical, identity_locked, local).

% Formal assemblies of bishops (Nicaea, Constantinople, Ephesus, Chalcedon, and subsequent councils) that convene to resolve doctrinal disputes and declare authoritative readings of Scripture. They function as the enforcement apparatus: their decrees are binding on communions that recognize them, and deviation becomes schism or heresy. Not agents in the narrow sense but institutional procedures that encode authority.
narrative_ontology:constraint_stakeholder(biblical_authority__conciliar_reading, ecumenical_councils, agenda_setter,
    institutional, generational, analytical, universal).
narrative_ontology:stakeholder_non_agent(biblical_authority__conciliar_reading, ecumenical_councils).

% Protestant reformers and their successors who would argue for Scripture's sufficiency and self-interpretation apart from patristic consensus or conciliar authority. They are excluded from the framework this constraint represents; their participation would require accepting the binding nature of councils and patristic consensus, which their own hermeneutic rejects. They would be trapped if they tried to stay: their core commitment (sola scriptura) is incompatible with conciliar gatekeeping.
narrative_ontology:constraint_stakeholder(biblical_authority__conciliar_reading, sola_scriptura_reformers, excluded,
    powerful, biographical, trapped, continental).

% Those (especially in Roman Catholicism post-Trent) who would argue for a living papal magisterium that can develop doctrine beyond what patristic consensus explicitly states. They are excluded from the conciliar reading's framework; this reading rejects magisterial decree as the operative form of authority, preferring collegial and consensus-based legitimation. Their exit involves joining the conciliar framework, which requires abandoning papal authority claims.
narrative_ontology:constraint_stakeholder(biblical_authority__conciliar_reading, magisterial_centralization_advocates, excluded,
    institutional, generational, constrained, continental).

% Orthodox, Oriental Orthodox, and other churches organized around the first ecumenical councils and patristic continuity. They benefit from having their identity and doctrine grounded in ancient, widely-recognized consensus rather than in the innovations of any single communion. The constraint validates their existing institutional form and hermeneutical practice. Their exit (abandoning councils) means institutional dissolution or merger with another tradition.
narrative_ontology:constraint_stakeholder(biblical_authority__conciliar_reading, historical_continuity_communities, beneficiary,
    organized, generational, mobile, continental).

% Academic and church-based scholars who study Scripture, patristic texts, and conciliar history. They take testimony and evidence from multiple readings (sola scriptura, conciliar, magisterial) and can shift frameworks or identify contradictions between authoritative texts and their purported consensus. They hold no power to enforce conciliar authority but can influence elite and educated believers' understanding of its historical validity.
narrative_ontology:constraint_stakeholder(biblical_authority__conciliar_reading, theological_scholars, observer,
    organized, biographical, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(biblical_authority__conciliar_reading, episcopal_collegiality).
narrative_ontology:fixing_cost_class(biblical_authority__conciliar_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem of doctrinal fragmentation and heresy: provides a shared hermeneutical apparatus (councils + patristic consensus) that allows dispersed bishops and communities to reach binding agreement on what Scripture teaches without appeal to a single central magisterium. The coordination cost is the constraint that novel readings must be legitimated retrospectively through patristic coherence rather than adopted prospectively. The first ecumenical councils (Nicaea, Constantinople) resolved major heresies (Arianism, etc.) that threatened communion; subsequent councils maintained doctrinal stability and prevented schism.
% TRANSFER_FUNCTION: Moves hermeneutical authority from individual interpreters and localized readings toward bishops organized in councils and the authority of the patristic tradition. Transfers doctrinal innovation capacity from rapid adaptation toward conservative, consensus-bound interpretation. What flows is control over legitimate reading: those in the conciliar-consensus frame gain it; those seeking doctrinal development lose the capacity to declare it without showing its ancient pedigree. The transfer flows from 'rapid_doctrinal_adaptation' and 'individual_scriptural_interpretation' toward 'episcopal_collegiality'.
% ABSENT_VOICES: Sola scriptura Protestants and magisterial-centralization advocates are absent. Protestants would argue that councils are fallible human bodies, not infallible interpreters, and that Scripture's meaning is determinate without patristic interpolation—this would lower the constraint's extractiveness and remove the retroactive-legitimation gate. Magisterial advocates would argue that doctrine must develop, not calcify, and that a living hierarchical authority is needed to shepherd that development—this would increase extraction intensity but shift it toward papal rather than episcopal seats. Also absent in formal theological discourse: lay believers in non-scholarly contexts who might read Scripture differently and encounter conciliar teaching as an external imposition rather than as a guide they have internalized.
% DISAPPEARANCE_RATIONALE: If the conciliar-consensus constraint vanished overnight, biblical interpretation would fragment across individual readers, local communities, and competing Protestant and Catholic frameworks within weeks to months. The historical ecumenical consensus would lose its binding character and become merely advisory. Doctrine would evolve faster in some traditions and regress in others depending on which framings each community adopted. The organizational coherence of Orthodox and Oriental Orthodox communions, which depend on recognizing the first councils as authoritative, would face immediate internal strain and schism risk. Magisterial authority would expand into the void (if Roman Catholicism expanded jurisdiction) or Protestant frameworks would gain ground in regions where conciliar authority had held. The entire structure of Christian unity grounded in 'reception' of councils would be dissolved.
% FOUNDING_PROBLEM: In the early Christian centuries (roughly 2nd–5th centuries), doctrinal disputes erupted (Arianism, Nestorianism, Monophysitism, Pelagianism, etc.) that threatened communion and posed existential risks to Christian unity. A mechanism was needed to settle what Scripture truly teaches when different interpreters reached different conclusions. The Nicene and subsequent councils, grounded in reference to patristic wisdom and apostolic tradition, provided that mechanism: bishops assembled to hear disputes, consulted the Fathers, and issued decrees that bound communions recognizing conciliar authority.
% FOUNDING_PROBLEM_CORROBORATION: Orthodox and Oriental Orthodox hierarchies explicitly attest the founding problem is still live: doctrinal fragmentation and schism remain risks in a world of autocephalous churches and ongoing theological disputes. Historical scholars (Pelikan, Meyendorff, Brown, et al.) and Protestant exegetes attest that the founding problem (doctrinal fragmentation into Arianism, Nestorianism, etc.) was substantially addressed by councils in the 4th–5th centuries but is no longer the operative threat; modern threats come from rationalism, scientism, and political ideology, not from Arian heresy. Catholic magisterial voices attest that the founding problem was partially solved by councils but remains incompletely solved, and that a living papal teaching authority is necessary for ongoing doctrinal development and application to modern contexts. A large body of secular historical scholarship (not claiming theological truth) attests that councils achieved political and institutional consolidation, whether or not they achieved doctrinal truth.
narrative_ontology:disappearance_verdict(biblical_authority__conciliar_reading, world_rearranges).
narrative_ontology:founding_problem_status(biblical_authority__conciliar_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(biblical_authority__conciliar_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(biblical_authority__conciliar_reading, 'none', 1).
narrative_ontology:epsilon_provenance(biblical_authority__conciliar_reading, 0.48, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(biblical_authority__conciliar_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(biblical_authority__conciliar_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(biblical_authority__conciliar_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness score (0.48 at interval end) reflects moderate clerical gatekeeping: bishops and councils do extract hermeneutical authority, but less than papal magisterium (sola_scriptura would produce lower extraction because it removes institutional gates entirely; tradition_scripture would produce higher extraction because papal decree is more centralized). Suppression rises from 0.35 to 0.52 over the interval, modeling the increasing institutional capacity of councils to enforce conciliar decrees (hardening of enforcement machinery post-Nicaea). Theater rises modestly from 0.28 to 0.41, reflecting that a growing share of conciliar activity defends the authority of councils themselves (metatheological theater) rather than resolving novel doctrinal problems. The temporal trajectory shows extraction and suppression stabilizing by t=25, suggesting the constraint reached institutional maturity in late antiquity and has maintained roughly stable force since. The measurements are authored on a shared time grid (every metric at every time point) to avoid misalignment.
 *
 * PERSPECTIVAL GAP:
 *   From the episcopal seat, the arrangement is genuine coordination: it solved the fragmentation problem, maintains communion, and preserves their collegial role. From the rapid-adaptation seat, it is constraining extraction: innovation is blocked unless retrospectively validated. From the sola_scriptura seat (excluded), it is illegitimate gatekeeping. From the magisterial seat (also excluded), it is insufficient and archaic. The engine computes divergence from the structural data: low d for beneficiaries produces negative χ (subsidy effect), high d for victims produces high χ (extraction effect). This reading-dependent computation is the proper locus of the perspectival gap; it is not reconciled in the authored claim.
 *
 * DIRECTIONALITY LOGIC:
 *   Episcopal collegiality sits at low d (0.25–0.35): the constraint legitimates their authority, they collect hermeneutical rents (the power to set and enforce interpretation), and their exit is mobile within the episcopal structure (though rupture with conciliar authority means schism). Rapid doctrinal adaptation sits at high d (0.75–0.85): it is the structural victim, constrained to show retrospective legitimation rather than prospective development, and faces identity lock (breaking patristic coherence means heresy, which is a form of exit that destroys the agent's identity within the tradition). Individual scriptural interpretation sits between (d ≈ 0.65–0.75): powerless agents face high suppression and identity lock, though they do receive coordination benefit from the clarity councils provide. The excluded seats (Protestants, magisterial advocates) have undefined d because they are not coordinated by this constraint; their presence in the excluded role confirms the constraint's enforcement boundaries.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (doctrinal fragmentation) is contested: Orthodox hierarchies attest it is still live; reformers attest councils are fallible; magisterial Catholicism attests councils are necessary but insufficient. The constraint shows no signs of mandatrophy resolved — it remains active in Orthodox polity and is still invoked (though weaker) in ecumenical dialogue. However, the theater_ratio's modest rise suggests growing metatheological self-defense (defending conciliar authority itself) rather than substantive doctrinal innovation. A future measurement showing theater > 0.6 or resistance dropping below 0.3 would suggest the constraint is operating primarily through performance rather than genuine coordination function, a signal of incipient mandatrophy. The measurement series does not yet show this; the constraint appears to be in stable operation with genuine coordination function, not yet degraded to Piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    patristic_consensus_boundary,
    'What constitutes ''patristic consensus''? How much divergence among the Fathers can be tolerated before the consensus breaks?',
    'Examine concrete historical disputes where councils invoked patristic consensus and where that invocation was contested. Map the actual variation in patristic opinion and compare to council declarations.',
    'If consensus is sharply defined and uniform, the constraint''s gatekeeping is tight and extraction high. If consensus is loose and accommodates wide variation, gatekeeping is permeable and extraction lower. The boundary affects whether rapid adaptation can find retrospective patristic support.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(patristic_consensus_boundary, empirical, 'How sharply bounded is the patristic consensus that councils invoke?').

omega_variable(
    conciliar_infallibility_claim,
    'Do councils have infallible authority to declare what Scripture teaches, or merely high credibility based on their antiquity and wisdom?',
    'Examine council declarations against historical evidence and contemporary scholarship; assess whether councils can err or whether error is ruled out by definition.',
    'If councils are infallible by dogmatic decree, the constraint''s extraction is supported by a metaphysical claim and resistance from non-believers should be modeled as identity-locked (they cannot accept infallibility). If councils are merely wise and credible, the constraint depends on ongoing acceptance and can face higher resistance from scholars who find errors.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(conciliar_infallibility_claim, conceptual, 'Whether conciliar authority is infallible by definition or merely highly credible by history.').

omega_variable(
    living_tradition_specification,
    'What makes the tradition ''living''? How does it evolve or develop while remaining conciliar and patristic?',
    'Trace instances where Orthodox and Oriental Orthodox churches have adopted practices or doctrines not explicitly stated in patristic texts or council decrees; determine whether these adoptions are claimed to be developments, rediscoveries, or something else.',
    'If ''living'' permits genuine development (new doctrines), the constraint''s extractiveness is lower because rapid adaptation can achieve evolution through reframing. If ''living'' means only deeper understanding of unchanging truths, extraction is higher because development is foreclosed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(living_tradition_specification, empirical, 'Whether the ''living tradition'' permits doctrinal development or only reinterpretation of fixed deposit.').

omega_variable(
    autocephalous_fragmentation_tension,
    'How do autocephalous churches maintain conciliar consensus when they lack a central authority to enforce it?',
    'Examine schism history (Great Schism, Oriental Orthodox separation, modern jurisdictional disputes) to determine whether conciliar consensus actually holds across autocephalous bodies or whether it fragments under pressure.',
    'If consensus is maintained without central enforcement, the constraint shows genuine coordination without hierarchy. If consensus fragments despite conciliar claims, the constraint''s effectiveness is lower and resistance higher.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(autocephalous_fragmentation_tension, empirical, 'Whether conciliar consensus is stable across autocephalous churches or prone to fragmentation.').

omega_variable(
    contested_kernel_underspecification,
    'Is the conciliar reading truly a different constraint from the tradition_scripture reading, or is it a nuanced version of it?',
    'Compare the two readings on their core claims: does conciliar reject living magisterium by definition, or does it merely emphasize collegiality over papal centralization? If the latter, the readings may be measuring different aspects of the same underlying structure rather than genuinely distinct constraints.',
    'If they are distinct, this story and the tradition_scripture story should produce different types and different victim/beneficiary sets. If they are variants on the same theme, the ε-invariance principle requires decomposing differently (perhaps by institutional form rather than by hermeneutical method).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(contested_kernel_underspecification, conceptual, 'Whether conciliar and tradition_scripture readings are distinct constraints or nuanced variants of the same commitment.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(biblical_authority__conciliar_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bibl_tr_t0, biblical_authority__conciliar_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(bibl_tr_t0, observed).
narrative_ontology:measurement(bibl_tr_t5, biblical_authority__conciliar_reading, theater_ratio, 5, 0.33).
narrative_ontology:measurement_basis(bibl_tr_t5, observed).
narrative_ontology:measurement(bibl_tr_t10, biblical_authority__conciliar_reading, theater_ratio, 10, 0.37).
narrative_ontology:measurement_basis(bibl_tr_t10, observed).
narrative_ontology:measurement(bibl_tr_t15, biblical_authority__conciliar_reading, theater_ratio, 15, 0.39).
narrative_ontology:measurement_basis(bibl_tr_t15, observed).
narrative_ontology:measurement(bibl_tr_t25, biblical_authority__conciliar_reading, theater_ratio, 25, 0.41).
narrative_ontology:measurement_basis(bibl_tr_t25, observed).
narrative_ontology:measurement(bibl_tr_t40, biblical_authority__conciliar_reading, theater_ratio, 40, 0.41).
narrative_ontology:measurement_basis(bibl_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(bibl_be_t0, biblical_authority__conciliar_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement_basis(bibl_be_t0, observed).
narrative_ontology:measurement(bibl_be_t5, biblical_authority__conciliar_reading, base_extractiveness, 5, 0.42).
narrative_ontology:measurement_basis(bibl_be_t5, observed).
narrative_ontology:measurement(bibl_be_t10, biblical_authority__conciliar_reading, base_extractiveness, 10, 0.45).
narrative_ontology:measurement_basis(bibl_be_t10, observed).
narrative_ontology:measurement(bibl_be_t15, biblical_authority__conciliar_reading, base_extractiveness, 15, 0.47).
narrative_ontology:measurement_basis(bibl_be_t15, observed).
narrative_ontology:measurement(bibl_be_t25, biblical_authority__conciliar_reading, base_extractiveness, 25, 0.48).
narrative_ontology:measurement_basis(bibl_be_t25, observed).
narrative_ontology:measurement(bibl_be_t40, biblical_authority__conciliar_reading, base_extractiveness, 40, 0.48).
narrative_ontology:measurement_basis(bibl_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(bibl_su_t0, biblical_authority__conciliar_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(bibl_su_t0, observed).
narrative_ontology:measurement(bibl_su_t5, biblical_authority__conciliar_reading, suppression_requirement, 5, 0.41).
narrative_ontology:measurement_basis(bibl_su_t5, observed).
narrative_ontology:measurement(bibl_su_t10, biblical_authority__conciliar_reading, suppression_requirement, 10, 0.46).
narrative_ontology:measurement_basis(bibl_su_t10, observed).
narrative_ontology:measurement(bibl_su_t15, biblical_authority__conciliar_reading, suppression_requirement, 15, 0.5).
narrative_ontology:measurement_basis(bibl_su_t15, observed).
narrative_ontology:measurement(bibl_su_t25, biblical_authority__conciliar_reading, suppression_requirement, 25, 0.52).
narrative_ontology:measurement_basis(bibl_su_t25, observed).
narrative_ontology:measurement(bibl_su_t40, biblical_authority__conciliar_reading, suppression_requirement, 40, 0.52).
narrative_ontology:measurement_basis(bibl_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(biblical_authority__conciliar_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(biblical_authority__conciliar_reading, 0.12).
narrative_ontology:affects_constraint(biblical_authority__conciliar_reading, biblical_authority__sola_scriptura_reading).
narrative_ontology:affects_constraint(biblical_authority__conciliar_reading, biblical_authority__tradition_scripture_reading).

% DUAL FORMULATION NOTE:
% The biblical_authority kernel decomposes into three structurally distinct constraint stories, each representing a different reading of Scripture's authoritative status and the role of tradition/councils/magisterium. The conciliar_reading (this story) holds that councils and patristic consensus are authoritative and live tradition is the medium of interpretation. The sola_scriptura_reading rejects councils and patristic gatekeeping, holding Scripture self-interpreting. The tradition_scripture_reading accepts councils but supplements them with living papal magisterium for doctrinal development. These readings have different ε values, different victim sets, and different institutional beneficiaries. They coexist in contemporary Christianity (held by different denominational families) rather than one logically foreclosing another. Constraint family decomposition follows DP-001 (ε-invariance): each reading instantiates a different claim about authority, with different empirical consequences and institutional structures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(biblical_authority__conciliar_reading, organized, 0.28).
constraint_indexing:directionality_override(biblical_authority__conciliar_reading, moderate, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
