% ============================================================================
% CONSTRAINT STORY: vatican_ii_authority__composite_overdetermination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vatican_ii_authority__composite_overdetermination_reading, []).

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
 *   constraint_id: vatican_ii_authority__composite_overdetermination_reading
 *   human_readable: Vatican II Authority Structure: Composite Overdetermination Reading
 *   domain: theological/ecclesiastical
 *
 * SUMMARY:
 *   Vatican II (1962-1965) is presented by the institutional magisterium as a
 *   coherent, continuous doctrinal development—one unified council with a
 *   single authoritative meaning binding on all Catholics. The
 *   composite-overdetermination reading rejects both this (false) continuity
 *   framing and the traditionalist rupture framing. Instead: Vatican II was
 *   an overdetermined composite of multiple incompatible doctrinal shifts
 *   produced by factional episcopal compromise, not by coherent theological
 *   development. The ambiguities that result cannot be resolved into either
 *   continuity or rupture because they encode genuine contradictions from the
 *   bargaining process. This reading benefits scholars (who can do honest
 *   analytical work on the tensions) and ecumenical theologians (who can
 *   engage with the actual changes), while imposing extraction costs on
 *   institutional authority (which must suppress the ambiguity to maintain
 *   its univocal reading), traditionalist clergy (who must implement reforms
 *   while maintaining institutional loyalty), and lay communities (who
 *   receive contradictory guidance). The measuring authority is the text of
 *   Vatican II itself, assessed by the composite-overdetermination reading's
 *   own lights: the referent is the standing institutional arrangement
 *   (Vatican II as the magisterium presents it), not the reading's endorsed
 *   alternative (honest acknowledgment of overdetermination).
 *
 * KEY AGENTS:
 *   - institutional_magisterium: agenda-setter, institutional power, civilizational horizon, identity-locked to the claim of univocal authority
 *   - scholarly_interpreters: beneficiaries, organized power, mobile exit, gain from explicit recognition of ambiguity
 *   - traditionalist_clergy: payers, moderate power, constrained exit (institutional discipline or schism), bear cost of enforced institutional coherence
 *   - lay_communities: payers, powerless, trapped exit, depend on univocal guidance but receive contradictory signals
 *   - ecumenical_theologians: beneficiaries, organized power, mobile exit, gain from honest engagement with actual doctrinal changes
 *   - traditionalist_movements: excluded, moderate power, constrained exit, have coherent alternative reading (Vatican II as rupture/heresy) kept out of official interpretation
 *   - reform_advocates: excluded, moderate power, constrained exit, have coherent alternative reading (Vatican II failed to go far enough) kept out of official interpretation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_authority__composite_overdetermination_reading, 0.68).
domain_priors:suppression_score(vatican_ii_authority__composite_overdetermination_reading, 0.72).
domain_priors:theater_ratio(vatican_ii_authority__composite_overdetermination_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_authority__composite_overdetermination_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(vatican_ii_authority__composite_overdetermination_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(vatican_ii_authority__composite_overdetermination_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vatican_ii_authority__composite_overdetermination_reading, accessibility_collapse, 0.61).
narrative_ontology:constraint_metric(vatican_ii_authority__composite_overdetermination_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_authority__composite_overdetermination_reading, tangled_rope).
narrative_ontology:human_readable(vatican_ii_authority__composite_overdetermination_reading, "Vatican II Authority Structure: Composite Overdetermination Reading").
narrative_ontology:topic_domain(vatican_ii_authority__composite_overdetermination_reading, "theological/ecclesiastical").

domain_priors:requires_active_enforcement(vatican_ii_authority__composite_overdetermination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_authority__composite_overdetermination_reading, '8e053037-0551-4a8e-85d1-4c828d71c8aa').
narrative_ontology:cs_kernel_codification('8e053037-0551-4a8e-85d1-4c828d71c8aa', fixed_text).
narrative_ontology:cs_authority_grounding('8e053037-0551-4a8e-85d1-4c828d71c8aa', extraction).
narrative_ontology:cs_interpretation_layer_present('8e053037-0551-4a8e-85d1-4c828d71c8aa').
narrative_ontology:cs_reading_relation('8e053037-0551-4a8e-85d1-4c828d71c8aa', vatican_ii_authority__continuity_reading, influences).
narrative_ontology:cs_reading_relation('8e053037-0551-4a8e-85d1-4c828d71c8aa', vatican_ii_authority__rupture_reading, coexists_with).
narrative_ontology:cs_axiom('8e053037-0551-4a8e-85d1-4c828d71c8aa', foundational, ambiguity_structural_not_resolvable).
narrative_ontology:cs_axiom_status(ambiguity_structural_not_resolvable, holdable).
narrative_ontology:cs_axiom_grounding('8e053037-0551-4a8e-85d1-4c828d71c8aa', ambiguity_structural_not_resolvable, empirically_contingent).
narrative_ontology:cs_axiom('8e053037-0551-4a8e-85d1-4c828d71c8aa', foundational, factional_compromise_produces_theological_contradiction).
narrative_ontology:cs_axiom_status(factional_compromise_produces_theological_contradiction, holdable).
narrative_ontology:cs_axiom_grounding('8e053037-0551-4a8e-85d1-4c828d71c8aa', factional_compromise_produces_theological_contradiction, empirically_contingent).
narrative_ontology:cs_axiom('8e053037-0551-4a8e-85d1-4c828d71c8aa', secondary, univocal_interpretation_institutional_performance).
narrative_ontology:cs_axiom_status(univocal_interpretation_institutional_performance, holdable).
narrative_ontology:cs_axiom_grounding('8e053037-0551-4a8e-85d1-4c828d71c8aa', univocal_interpretation_institutional_performance, instrumental).
narrative_ontology:cs_reference_frame('8e053037-0551-4a8e-85d1-4c828d71c8aa', vatican_ii_as_institutional_authority_unified_text).
narrative_ontology:cs_drift_state('8e053037-0551-4a8e-85d1-4c828d71c8aa', contemporary_scholarly_consensus_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('8e053037-0551-4a8e-85d1-4c828d71c8aa', '').
narrative_ontology:cs_kernel_id(vatican_ii_authority__composite_overdetermination_reading, vatican_ii_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_authority__composite_overdetermination_reading, scholarly_interpreters).
narrative_ontology:constraint_beneficiary(vatican_ii_authority__composite_overdetermination_reading, ecumenical_theologians).
narrative_ontology:constraint_victim(vatican_ii_authority__composite_overdetermination_reading, institutional_magisterium).
narrative_ontology:constraint_victim(vatican_ii_authority__composite_overdetermination_reading, traditionalist_clergy).
narrative_ontology:constraint_victim(vatican_ii_authority__composite_overdetermination_reading, lay_communities_seeking_univocal_guidance).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(vatican_ii_authority__composite_overdetermination_reading, vatican_official_interpreters).
narrative_ontology:constraint_vindicates(vatican_ii_authority__composite_overdetermination_reading, hermeneutical_complexity_thesis).
narrative_ontology:constraint_vindicates(vatican_ii_authority__composite_overdetermination_reading, factional_compromise_as_doctrine_production).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The Vatican's teaching authority (Pope, Curia, bishops in communion with Rome) claims exclusive interpretive authority over Vatican II's meaning and binding implementation. It presents Vatican II as univocally continuous and binding, settles disputes through magisterial pronouncements, and enforces this reading through disciplinary mechanisms (theological censure, book bans, removal from teaching positions). The institutional magisterium's legitimacy depends on the claim that it can authoritatively resolve interpretive disputes—a claim that requires suppressing the possibility that Vatican II's ambiguities are structural and unresolvable.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__composite_overdetermination_reading, institutional_magisterium, agenda_setter,
    institutional, civilizational, identity_locked, universal).

% Catholic and non-Catholic theologians, historians, and philologists study Vatican II's texts, drafting process, and effects. The composite-overdetermination reading is the consensus scholarly position: Vatican II was an overdetermined compromise whose texts genuinely support multiple interpretations. Scholars benefit by being freed from the institutional requirement to present Vatican II as univocally coherent; they can do honest textual analysis, trace factional influences on the final texts, and examine how different bishops understood the same words differently. Most are insulated from institutional discipline because they work in secular universities or non-ecclesial positions; a few in Catholic institutions face lower-grade pressure (not tenure denial, but restricted voice in official Church discourse).
narrative_ontology:constraint_stakeholder(vatican_ii_authority__composite_overdetermination_reading, scholarly_interpreters, beneficiary,
    organized, generational, mobile, global).

% Conservative priests and bishops who believe pre-Vatican II theology was superior (and Vatican II either ruptures with tradition or represents indefensible compromise) are required to implement reformed liturgy, updated theology, and new pastoral practices while maintaining institutional loyalty and public affirmation of Vatican II. They experience profound cognitive dissonance: their genuine theological convictions contradict what they are publicly required to teach and enact. Exit options are constrained: leaving the priesthood means loss of identity, community, and livelihood; joining traditionalist schism (SSPX) means institutional excommunication and loss of standing in the larger Church. They pay the cost of enforced institutional coherence through internalized contradiction.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__composite_overdetermination_reading, traditionalist_clergy, payer,
    moderate, biographical, constrained, national).

% Parish-level Catholics seek clear, authoritative guidance on faith, morals, and practice. Vatican II's ambiguities reach them as contradictory signals: official statements claim Vatican II represents continuity, yet the lived experience is one of massive change (vernacular liturgy instead of Latin, lay participation instead of priest-centered worship, married deacons, contraception acceptance rising despite official teaching). Some priests teach the old way, others the new; catechesis materials conflict; parishes that restored Latin Mass without episcopal permission are disciplined, signaling that continuity is not genuine. Laypeople lack the theological training to resolve the ambiguities independently and experience the constraint as institutional incoherence. They are trapped: they cannot exit Catholicism without losing their religious identity, community, and sense of belonging.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__composite_overdetermination_reading, lay_communities_seeking_univocal_guidance, payer,
    powerless, biographical, trapped, local).

% Protestant, Orthodox, Anglican, and reform-Catholic theologians engaged in ecumenical dialogue benefit from the composite-overdetermination reading: it enables honest conversation about what Vatican II actually changed, avoids the false choice between 'Vatican II is univocally continuous' (continuity reading) and 'Vatican II is illegitimately rupturing' (rupture reading), and permits acknowledgment of real theological divergences without requiring agreement on an impossible univocal reading. They can say: 'Vatican II genuinely shifted Catholic doctrine on religious freedom, ecumenism, liturgy, and the laity—this is historically accurate—and the post-conciliar Church has spent 60 years managing that change.' This reading frees ecumenical conversation from the requirement to pretend Vatican II left Catholic doctrine unchanged.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__composite_overdetermination_reading, ecumenical_theologians, beneficiary,
    organized, generational, mobile, global).

% Bishops and Vatican officials tasked with implementing Vatican II and defending its interpretation must present a univocal, unified reading despite intimate knowledge of the documents' internal tensions. They are aware—from their own reading, from theological advisors, from scholarly literature—that Vatican II contains genuine ambiguities and that different bishops at the council intended different things by the same words. Yet they must suppress this awareness to maintain institutional authority to settle disputes. They pay the cost of institutional cognitive dissonance: maintaining an impossible claim of coherence. Their role depends on that suppression; if they admitted the ambiguities were structural, their authority to authoritatively interpret would collapse.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__composite_overdetermination_reading, vatican_official_interpreters, agenda_setter,
    institutional, civilizational, identity_locked, universal).
narrative_ontology:stakeholder_secondary_role(vatican_ii_authority__composite_overdetermination_reading, vatican_official_interpreters, payer).

% Communities (Society of St. Pius X / SSPX, sede vacantist groups, other traditionalist organizations) that explicitly reject Vatican II's validity or teaching are structurally excluded from the official interpretation process. They have a coherent reading (Vatican II represents rupture or heresy, so Vatican II is invalid or should be repudiated), but this reading is not permitted in institutional Church forums. Their exclusion is intentional: institutional authority depends on closing the interpretive circle to those who would challenge Vatican II's binding character. If traditionalist communities were given a seat at the table, they would argue that Vatican II's overdetermination proves it is illegitimate—a reading that would destabilize the magisterium's claim to univocal authority.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__composite_overdetermination_reading, traditionalist_movements, excluded,
    moderate, generational, constrained, global).

% Progressive Catholics and reform theologians (those arguing Vatican II should have gone further, or that its ambiguities expose its fundamental inadequacy) are also excluded from shaping official interpretation. They have a coherent reading (Vatican II's overdetermination reveals that the council failed to address root problems adequately, and further reform is needed), but this reading is suppressed within official Church discourse. If permitted, they would argue that the composite-overdetermination reading proves the council is inadequate and that more radical change is necessary—a position that challenges the magisterium's presentation of Vatican II as the solution to modern Church problems.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__composite_overdetermination_reading, reform_advocates, excluded,
    moderate, generational, constrained, global).

% The historical record of prior ecumenical councils (Nicaea, Chalcedon, Lateran IV, Trent, etc.) provides analytical context for assessing Vatican II. Prior councils also produced interpretive disputes and texts that supported multiple readings over time. The composite-overdetermination reading claims Vatican II's ambiguities are structurally distinctive: they encode genuine theological contradictions from factional compromise, not merely normal conciliar ambiguity. Comparison with prior councils tests whether Vatican II's ambiguities exceed normal conciliar variation or exemplify it.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__composite_overdetermination_reading, historical_councils_precedent, observer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(vatican_ii_authority__composite_overdetermination_reading, historical_councils_precedent).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vatican_ii_authority__composite_overdetermination_reading, institutional_magisterium).
narrative_ontology:fixing_cost_class(vatican_ii_authority__composite_overdetermination_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Vatican II is presented as coordinating the Church's unified interpretation of doctrine and pastoral practice across the global institution: one authorized reading of what the council means, binding on all clergy and faithful. The coordination problem it ostensibly solves is: how does a universal Church maintain doctrinal coherence across massive cultural and linguistic diversity while updating doctrine for the modern world?
% TRANSFER_FUNCTION: Authority flows from the institutional magisterium (Vatican officials and bishops) to local implementation structures (parish clergy, seminaries, lay education). Control over interpretation flows upward: the magisterium claims exclusive authority to settle disputes about Vatican II's meaning. What is transferred to scholars and theological consensus-builders is legitimacy—but only the legitimacy to explain, not to authorize; the magisterium retains the gate.
% ABSENT_VOICES: Traditionalist movements and reform-advocate movements are structurally excluded from the conversation about Vatican II's meaning, despite having coherent alternative readings (traditionalists: the council represents rupture or heresy; reformers: the council failed to address root problems). Their exclusion is not accidental—institutional authority depends on closing the circle to defend univocal interpretation. Also absent: the voices of Vatican II's non-episcopal participants (theologians, lay auditors, women advisors) who were present at drafting but had no formal vote, and whose interpretations were often overridden by the final institutional compromise texts.
% DISAPPEARANCE_RATIONALE: If the composite-overdetermination reading disappeared—if Vatican II were successfully reframed as univocally continuous or unambiguously rupturing—the Church's institutional authority structure would stabilize; the grounds for post-conciliar conflict would dissolve into predictable factions (continuists vs. rupturists) rather than remaining as irresolvable theological contradictions within every faction. Traditionalist movements would have clearer grounds to claim the council is illegitimate; reform movements would have clearer grounds to claim it failed. The scholarly apparatus for managing ambiguity would lose its primary object. Institutional authority would regain the appearance of coherence, though the underlying reality would not change.
% FOUNDING_PROBLEM: Vatican II (1962-1965) was called to update the Church for the modern world while maintaining doctrinal continuity. The council was produced by contested bargaining among episcopal factions with incompatible theological premises: some bishops understood doctrinal development as organic unfolding of unchanging deposit; others understood it as substantive change in light of historical contingency; still others sought compromise texts that pleased both camps. The result: doctrine statements with internal tensions, metaphors that support contradictory readings, and compromises that shifted rather than resolved underlying disagreements.
% FOUNDING_PROBLEM_CORROBORATION: Vatican II historians (Alberigo, Wills, Pesch, O'Malley, and non-Catholic scholars like Barbara Duden) document the factional bargaining and textual compromise strategies that produced the documents. The magisterium itself attests the founding problem (updating for modernity while maintaining continuity) but claims it has been solved; scholars attest it has been produced but not solved. Cardinal Kasper (inside the institutional framework) and international theological commissions have acknowledged specific tensions in Vatican II's texts, though without abandoning the continuity framework. Outside the benefiting parties: academic consensus in religious studies and history departments treats Vatican II as an overdetermined compromise that generated irresolvable ambiguities. Traditionalist and reform-advocate movements corroborate the founding problem but dispute the institutional reading of its status.
narrative_ontology:disappearance_verdict(vatican_ii_authority__composite_overdetermination_reading, world_rearranges).
narrative_ontology:founding_problem_status(vatican_ii_authority__composite_overdetermination_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vatican_ii_authority__composite_overdetermination_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(vatican_ii_authority__composite_overdetermination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vatican_ii_authority__composite_overdetermination_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vatican_ii_authority__composite_overdetermination_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vatican_ii_authority__composite_overdetermination_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vatican_ii_authority__composite_overdetermination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness starts at 0.45 (1965) because immediately post-conciliar, the magisterium's framing of univocal continuity is still somewhat credible—Vatican II's text is fresh, the interpretive battles have not yet hardened, and the changes are still being explained as organic development. As time passes, the incongruence between the claimed continuity and observable institutional reality (liturgical change, theological shifts in practice) becomes undeniable. Extractiveness rises to 0.68 by 2025 because maintaining the univocal continuity reading now requires active suppression of contradictory evidence (textual analysis, historical scholarship, internal church conflicts). Theater ratio rises from 0.28 to 0.58 over the same period: increasingly, institutional maintenance of the continuity narrative becomes performative—ceremonial reaffirmations of Vatican II's univocal meaning, despite widespread scholarly and pastoral recognition that such univocity is not supported by the text. Suppression rises from 0.48 to 0.72 as the magisterium must more actively exclude alternative readings (traditionalist and reform-advocate interpretations) and discipline scholars whose work renders the ambiguities undeniable (Hans Küng's theological censure, etc.). The constraint is Tangled Rope because: (1) genuine coordination function exists (Vatican II coordinates global church practice and doctrine), (2) asymmetric extraction occurs (institutional authority extracts the benefit of coherence while scholars, traditionalists, and laypeople bear the cost of suppressed ambiguity), and (3) active enforcement is required (the magisterium must continuously police the univocal reading).
 *
 * PERSPECTIVAL GAP:
 *   The magisterium claims Vatican II represents continuous doctrinal development and that its authority to interpret the council's meaning is univocal. From the magisterium's seat, the constraint functions as pure coordination: it successfully presents a unified Church to the world, settles disputes by institutional fiat, and maintains doctrinal authority. From traditionalist and lay seats, the same constraint functions as asymmetric extraction: they are required to enact changes they theologically reject while pretending the changes are continuous with unchanging doctrine. From scholarly seats, the constraint functions as coordination (it solved the problem of how a modern Church can update doctrine) with a suppressed recognition of ambiguity (scholars benefit from acknowledging what institutional authority suppresses). The directionality divergence follows from the structural facts: who controls the interpretation gate (institutional magisterium), who pays the cost of univocal framing (traditionalists and laypeople), and who benefits from explicit ambiguity (scholars and ecumenical theologians).
 *
 * DIRECTIONALITY LOGIC:
 *   Institutional magisterium: d = 0.0-0.2 (full beneficiary). It sets the agenda, controls interpretation, collects the authority-legitimacy benefit of univocal doctrine. Even though scholars challenge its reading, the institutional seat maintains control over which readings are 'orthodox.' Scholarly interpreters and ecumenical theologians: d = 0.1-0.3 (beneficiaries). They gain research opportunity, institutional positioning, intellectual freedom without discipline. Their exit is mobile (they can publish or teach outside Catholic institutions; no direct career loss from the reading); they face zero suppression from institutional authority for this interpretation. Traditionalist clergy and lay communities: d = 0.8-1.0 (targets). Traditionalist clergy pay the cost of enforced institutional coherence (implement reformed practices while maintaining pre-conciliar theology, or face discipline/schism). Lay communities pay the cost of contradictory guidance and theological uncertainty. Their exit is constrained (clergy are identity-locked to priesthood; laypeople are identity-locked to parish community and Catholic identity). The constraint extracts from them by requiring them to maintain cognitive dissonance.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resolves the mandatrophy question by reframing Vatican II as an overdetermined compromise that CANNOT be resolved into univocal continuity OR rupture. The founding problem (how to update doctrine for modernity while maintaining continuity) has not been solved; it has been concealed by institutional framing. The institutional magisterium claims the founding problem is solved (Vatican II achieved continuous development), but scholars attest it is not solved—the ambiguities are structural, not resolvable through better interpretation. The founding problem's status is therefore 'dead as a solved problem, undead as a concealed problem.' The composite-overdetermination reading prevents the false classification of Vatican II as a 'solved' Rope (successful coordination) by documenting that the coordination only appears unified through suppressed ambiguity and disciplined interpretation. It also prevents the rupture reading from claiming the ambiguities prove Vatican II is incoherent heresy—instead, they prove Vatican II was a compromise that produced genuine, unresolvable tensions. The constraint is properly classified as Tangled Rope: real coordination achieved (the Church did update and remains institutionally unified), but at the cost of suppressed ambiguity and enforced univocal interpretation that extracts cognitive dissonance costs from traditionalists and laypeople while benefiting institutional authority and scholars. The theater ratio's rise (from 0.28 to 0.58) confirms the reading: as the ambiguities become undeniable, institutional maintenance of the univocal narrative becomes increasingly performative rather than substantive.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    authenticity_of_compromise_vs_organic_development,
    'Are Vatican II''s ambiguities the result of factional compromise (the composite-overdetermination reading''s claim), or are they the inevitable signature of organic doctrinal development (the continuity reading''s counter-claim)?',
    'Historical analysis of Vatican II''s drafting process, examination of voting records and conciliar debate transcripts, reconstruction of what each episcopal faction actually intended vs. what compromise texts achieved. Comparison with other councils'' drafting processes to establish whether compromise-produced ambiguity is distinctively high.',
    'If the ambiguities can be traced to identifiable factional bargaining (progressive bishops vs. conservative bishops, each forcing the text to do different work), the composite-overdetermination reading is strengthened and the continuity reading is weakened. If the ambiguities can be shown to be inherent to any attempt to update doctrine while maintaining continuity, the continuity reading gains credibility and the overdetermination reading is partially reframed as a normal feature of doctrinal development, not a structural problem.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(authenticity_of_compromise_vs_organic_development, empirical, 'Whether Vatican II''s ambiguities arise from factional compromise or organic theological development.').

omega_variable(
    suppression_mechanism_structural_or_internalized,
    'Is the institutional magisterium''s suppression of Vatican II''s ambiguities a structural consequence of institutional authority (the institution cannot admit ambiguity without undermining its authority to settle disputes), or an internalized choice by bishops and officials who genuinely believe in univocal continuity?',
    'Deathbed interviews and private correspondence from Vatican officials and bishops; analysis of what bishops say in private forums vs. public statements; examination of bishops'' own working documents and theology vs. their official statements on Vatican II.',
    'If suppression is structural (institutional authority requires univocal interpretation), the constraint is necessarily Tangled Rope and the suppression is amplified by institutional identity-lock. If suppression is internalized (officials genuinely believe in continuity), the constraint might be reclassified as Rope (coordination without intentional extraction) and the theater ratio would be reinterpreted as honest confusion rather than performance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_or_internalized, empirical, 'Whether suppression of Vatican II''s ambiguities is a structural feature of institutional authority or an internalized belief.').

omega_variable(
    alternative_reading_feasibility,
    'If the institutional magisterium dropped the univocal continuity claim and explicitly acknowledged Vatican II as an overdetermined compromise, could the Church maintain institutional coherence and authority, or would the resulting institutional instability require suppression to be reinstated?',
    'Counterfactual: if one pope (hypothetically) published a document acknowledging Vatican II''s ambiguities as structural and unresolvable, what would be the institutional fallout? Would traditionalist movements escalate or stabilize? Would lay communities lose confidence or gain trust? Would scholarly work become more productive or more fragmented?',
    'If institutional coherence requires the univocal framing (instability would spike if it were abandoned), then suppression is not negotiable and the constraint is necessarily Tangled Rope. If institutional coherence could be maintained through honest acknowledgment of ambiguity (by repositioning authority as ''guide through ambiguity'' rather than ''resolver of ambiguity''), the constraint might transition to Rope and the extraction costs would decrease.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_reading_feasibility, conceptual, 'Whether institutional coherence depends on univocal interpretation of Vatican II or can survive explicit acknowledgment of ambiguity.').

omega_variable(
    kernel_readable_as_forecloses_relationship,
    'Does the composite-overdetermination reading (the reading you are authoring) foreclose the continuity_reading and rupture_reading, or do all three readings coexist as live positions held by different parties?',
    'Test the logical structure: can a party hold the composite-overdetermination view AND hold the continuity view as true? If yes, the readings coexist; if no (i.e., accepting overdetermination logically requires rejecting continuity), then forecloses. The test is: can the institutional magisterium acknowledge Vatican II as an overdetermined compromise AND claim continuity? The magisterium''s current claim is ''Vatican II is univocal and continuous''—incompatible with ''Vatican II is overdetermined and unresolvable.'' But could the magisterium reframe to ''Vatican II represents continuous development WITHIN the ambiguity''?',
    'If the readings logically foreclose each other, the reading_relations are forecloses (rare). If they remain live alternatives held by different theological factions, reading_relations are coexists_with. If this reading creates structural pressure that makes sibling readings harder to maintain (e.g., the scholarly dominance of the overdetermination reading makes the continuity reading less credible institutionally), the relation is influences.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_readable_as_forecloses_relationship, conceptual, 'The logical relationship between composite-overdetermination_reading and its sibling readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_authority__composite_overdetermination_reading, 1965, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vati_tr_t1965, vatican_ii_authority__composite_overdetermination_reading, theater_ratio, 1965, 0.28).
narrative_ontology:measurement_basis(vati_tr_t1965, observed).
narrative_ontology:measurement(vati_tr_t1978, vatican_ii_authority__composite_overdetermination_reading, theater_ratio, 1978, 0.35).
narrative_ontology:measurement_basis(vati_tr_t1978, observed).
narrative_ontology:measurement(vati_tr_t1995, vatican_ii_authority__composite_overdetermination_reading, theater_ratio, 1995, 0.48).
narrative_ontology:measurement_basis(vati_tr_t1995, observed).
narrative_ontology:measurement(vati_tr_t2010, vatican_ii_authority__composite_overdetermination_reading, theater_ratio, 2010, 0.54).
narrative_ontology:measurement_basis(vati_tr_t2010, observed).
narrative_ontology:measurement(vati_tr_t2020, vatican_ii_authority__composite_overdetermination_reading, theater_ratio, 2020, 0.58).
narrative_ontology:measurement_basis(vati_tr_t2020, observed).
narrative_ontology:measurement(vati_tr_t2025, vatican_ii_authority__composite_overdetermination_reading, theater_ratio, 2025, 0.58).
narrative_ontology:measurement_basis(vati_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(vati_be_t1965, vatican_ii_authority__composite_overdetermination_reading, base_extractiveness, 1965, 0.45).
narrative_ontology:measurement_basis(vati_be_t1965, observed).
narrative_ontology:measurement(vati_be_t1978, vatican_ii_authority__composite_overdetermination_reading, base_extractiveness, 1978, 0.52).
narrative_ontology:measurement_basis(vati_be_t1978, observed).
narrative_ontology:measurement(vati_be_t1995, vatican_ii_authority__composite_overdetermination_reading, base_extractiveness, 1995, 0.61).
narrative_ontology:measurement_basis(vati_be_t1995, observed).
narrative_ontology:measurement(vati_be_t2010, vatican_ii_authority__composite_overdetermination_reading, base_extractiveness, 2010, 0.65).
narrative_ontology:measurement_basis(vati_be_t2010, observed).
narrative_ontology:measurement(vati_be_t2020, vatican_ii_authority__composite_overdetermination_reading, base_extractiveness, 2020, 0.68).
narrative_ontology:measurement_basis(vati_be_t2020, observed).
narrative_ontology:measurement(vati_be_t2025, vatican_ii_authority__composite_overdetermination_reading, base_extractiveness, 2025, 0.68).
narrative_ontology:measurement_basis(vati_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(vati_su_t1965, vatican_ii_authority__composite_overdetermination_reading, suppression_requirement, 1965, 0.48).
narrative_ontology:measurement_basis(vati_su_t1965, observed).
narrative_ontology:measurement(vati_su_t1978, vatican_ii_authority__composite_overdetermination_reading, suppression_requirement, 1978, 0.58).
narrative_ontology:measurement_basis(vati_su_t1978, observed).
narrative_ontology:measurement(vati_su_t1995, vatican_ii_authority__composite_overdetermination_reading, suppression_requirement, 1995, 0.66).
narrative_ontology:measurement_basis(vati_su_t1995, observed).
narrative_ontology:measurement(vati_su_t2010, vatican_ii_authority__composite_overdetermination_reading, suppression_requirement, 2010, 0.71).
narrative_ontology:measurement_basis(vati_su_t2010, observed).
narrative_ontology:measurement(vati_su_t2020, vatican_ii_authority__composite_overdetermination_reading, suppression_requirement, 2020, 0.72).
narrative_ontology:measurement_basis(vati_su_t2020, observed).
narrative_ontology:measurement(vati_su_t2025, vatican_ii_authority__composite_overdetermination_reading, suppression_requirement, 2025, 0.72).
narrative_ontology:measurement_basis(vati_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_authority__composite_overdetermination_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(vatican_ii_authority__composite_overdetermination_reading, 0.22).
narrative_ontology:affects_constraint(vatican_ii_authority__composite_overdetermination_reading, vatican_ii_authority__continuity_reading).
narrative_ontology:affects_constraint(vatican_ii_authority__composite_overdetermination_reading, vatican_ii_authority__rupture_reading).
narrative_ontology:affects_constraint(vatican_ii_authority__composite_overdetermination_reading, magisterial_authority_structure).
narrative_ontology:affects_constraint(vatican_ii_authority__composite_overdetermination_reading, post_conciliar_liturgical_implementation).
narrative_ontology:affects_constraint(vatican_ii_authority__composite_overdetermination_reading, traditionalist_schism_boundary).

% DUAL FORMULATION NOTE:
% The vatican_ii_authority kernel has three constraint stories: continuity_reading (institutional magisterium's framing; claims univocal continuity), rupture_reading (traditionalist framing; claims Vatican II represents doctrinal break), and composite_overdetermination_reading (scholarly framing; claims ambiguities are unresolvable because the council encoded incompatible theological premises). All three share the same referent (Vatican II's institutional authority and doctrinal content) but author different ε values and stakeholder structures. Continuity_reading: low ε (presents Vatican II as successful coordination). Rupture_reading: high ε (presents Vatican II as illegitimate or incoherent). Composite_overdetermination_reading: medium-high ε (presents Vatican II as real coordination achieved through suppressed ambiguity). They are NOT perspectives on one constraint; they are three structurally distinct constraints arising from three different readings of the same kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(vatican_ii_authority__composite_overdetermination_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
