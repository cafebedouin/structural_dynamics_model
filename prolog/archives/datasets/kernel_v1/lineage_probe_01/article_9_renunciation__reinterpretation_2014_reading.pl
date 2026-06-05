% ============================================================================
% CONSTRAINT STORY: article_9_renunciation__reinterpretation_2014_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_9_renunciation__reinterpretation_2014_reading, []).

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
 *   constraint_id: article_9_renunciation__reinterpretation_2014_reading
 *   human_readable: Article 9 Constitutional Reinterpretation (2014 Cabinet Decision)
 *   domain: constitutional_law/executive_power
 *
 * SUMMARY:
 *   The 2014 cabinet decision reinterpreting Article 9 of Japan's 1947
 *   Constitution represents a case of constitutional meaning being altered by
 *   executive fiat rather than through the formal amendment process specified
 *   in Article 96 (requiring two-thirds supermajority in both chambers of the
 *   Diet). For 67 years, the constitutionally official reading held that
 *   Article 9's renunciation of war prevented Japan from exercising the right
 *   of collective self-defense — Japan could defend itself but could not
 *   defend allies militarily. The 2014 reinterpretation crossed this standing
 *   line: the cabinet authorized collective self-defense through a unilateral
 *   reading change, approved by cabinet decision and announced to the Diet,
 *   never submitted to Article 96's amendment gate. The constraint is the
 *   extraction of constitutional authority — changing the supreme law at
 *   cabinet price rather than supermajority cost. The structural data shows
 *   moderate-to-high extractiveness (0.58), high suppression (0.72: the
 *   amendment path is suppressed by executive fiat, Diet supermajorities are
 *   rendered decorative), and moderate-high theater (0.68: the
 *   reinterpretation is partly substantive coordination, partly performative
 *   assertion of executive meaning-settling authority). This reading
 *   instantiates one interpretation of the contested Article 9 kernel — the
 *   kernel being the text's fixed meaning and its proper mode of revision.
 *   This reading holds that cabinet reinterpretation is a legitimate (if
 *   extractive) mode of constitutional change when formal amendment is
 *   blocked by political deadlock.
 *
 * KEY AGENTS:
 *   - Reinterpreting Executive (Cabinet / Prime Minister Abe): Institutional beneficiary — exercises novel constitutional authority, bypasses amendment requirement, clarifies alliance commitment without supermajority friction.
 *   - Security Alliance (US bilateral alliance, NATO interop): Institutional beneficiary — collective defense operationalized, alliance clarity improved, strategic ambiguity removed.
 *   - Formal Amendment Procedure (Article 96 supermajority gate): Primary victim (powerless/trapped) — structural authority is bypassed, two-thirds requirement rendered decorative, the amendment path's exclusivity is violated.
 *   - Diet Supermajority (Legislative minority unable to support amendment): Secondary victim (moderate/constrained) — pacifist or cautious coalition cannot block reinterpretation via amendment because executive has already acted; forced to either acquiesce or mount reversal effort.
 *   - Pacifist Constitutional Identity (civil society, opposition parties, courts-watching progressives): Identity-locked victim — 67 years of constitutionalized pacifism reversed; identity-fused agents experience this as constitutional betrayal, not policy disagreement.
 *   - Analytical Observer (constitutional scholars, institutional designers): Sees natural law boundary (text is fixed meaning, amendment is the only legitimate path) but this masks the extractive mechanism — a false summit that naturalizes executive power aggrandizement.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_9_renunciation__reinterpretation_2014_reading, 0.58).
domain_priors:suppression_score(article_9_renunciation__reinterpretation_2014_reading, 0.72).
domain_priors:theater_ratio(article_9_renunciation__reinterpretation_2014_reading, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_9_renunciation__reinterpretation_2014_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(article_9_renunciation__reinterpretation_2014_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(article_9_renunciation__reinterpretation_2014_reading, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_9_renunciation__reinterpretation_2014_reading, tangled_rope).
narrative_ontology:human_readable(article_9_renunciation__reinterpretation_2014_reading, "Article 9 Constitutional Reinterpretation (2014 Cabinet Decision)").
narrative_ontology:topic_domain(article_9_renunciation__reinterpretation_2014_reading, "constitutional_law/executive_power").

domain_priors:requires_active_enforcement(article_9_renunciation__reinterpretation_2014_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_9_renunciation__reinterpretation_2014_reading, 'df504cc4-9912-4f99-8769-8d621dda5a23').
narrative_ontology:cs_kernel_codification('df504cc4-9912-4f99-8769-8d621dda5a23', fixed_text).
narrative_ontology:cs_authority_grounding('df504cc4-9912-4f99-8769-8d621dda5a23', extraction).
narrative_ontology:cs_interpretation_layer_present('df504cc4-9912-4f99-8769-8d621dda5a23').
narrative_ontology:cs_reading_relation('df504cc4-9912-4f99-8769-8d621dda5a23', article_9_renunciation__absolute_pacifism_reading, coexists_with).
narrative_ontology:cs_reading_relation('df504cc4-9912-4f99-8769-8d621dda5a23', article_9_renunciation__self_defense_interpretation_reading, influences).
narrative_ontology:cs_axiom('df504cc4-9912-4f99-8769-8d621dda5a23', foundational, interpretation_can_settle_constitutional_meaning).
narrative_ontology:cs_axiom_status(interpretation_can_settle_constitutional_meaning, holdable).
narrative_ontology:cs_axiom_grounding('df504cc4-9912-4f99-8769-8d621dda5a23', interpretation_can_settle_constitutional_meaning, conventional).
narrative_ontology:cs_axiom('df504cc4-9912-4f99-8769-8d621dda5a23', secondary, coordination_cost_justifies_procedural_bypass).
narrative_ontology:cs_axiom_status(coordination_cost_justifies_procedural_bypass, holdable).
narrative_ontology:cs_axiom_grounding('df504cc4-9912-4f99-8769-8d621dda5a23', coordination_cost_justifies_procedural_bypass, instrumental).
narrative_ontology:cs_reference_frame('df504cc4-9912-4f99-8769-8d621dda5a23', formal_amendment_supremacy).
narrative_ontology:cs_drift_state('df504cc4-9912-4f99-8769-8d621dda5a23', post_2014_cabinet_decision, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('df504cc4-9912-4f99-8769-8d621dda5a23', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(article_9_renunciation__reinterpretation_2014_reading, article_9_renunciation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_9_renunciation__reinterpretation_2014_reading, security_alliance_operability).
narrative_ontology:constraint_beneficiary(article_9_renunciation__reinterpretation_2014_reading, reinterpreting_executive).
narrative_ontology:constraint_victim(article_9_renunciation__reinterpretation_2014_reading, formal_amendment_procedure).
narrative_ontology:constraint_victim(article_9_renunciation__reinterpretation_2014_reading, constitutional_supermajority_gate).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FORMAL AMENDMENT PROCEDURE (SNARE) — The Article 96 supermajority requirement (two-thirds in both chambers) has been circumvented by cabinet reinterpretation. This agent has no exit. The formal path to constitutional change is structurally trapped by the executive's ability to amend by decree. Maximum experienced extraction — the gate is crossed, the procedure is bypassed, the supermajority is rendered decorative.
constraint_indexing:constraint_classification(article_9_renunciation__reinterpretation_2014_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: DIET SUPERMAJORITY / LEGISLATIVE MINORITY (TANGLED ROPE) — Constrained by the fact that challenging the reinterpretation requires either a new supermajority to amend explicitly or court reversal (both costly). Yet the minority also benefits from the reinterpretation's coordination function: collective defense becomes operationalized, alliance commitment is clarified, and the legislative body is freed from the deadlock that pure Article 9 created. The constraint mixes extraction (power stripped by executive fiat) with coordination (deadlock broken).
constraint_indexing:constraint_classification(article_9_renunciation__reinterpretation_2014_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: SECURITY ALLIANCE OPERABILITY (ROPE) — Benefits from the reinterpretation as genuine coordination: collective defense is now operational, alliance commitments are clarified, and the prior ambiguity is resolved. This agent experiences the constraint as problem-solving (moving from constitutional paralysis to operational clarity). The reinterpretation solves a coordination failure for this agent.
constraint_indexing:constraint_classification(article_9_renunciation__reinterpretation_2014_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REINTERPRETING EXECUTIVE (ROPE) — Experiences the reinterpretation as pure coordination: the executive clarifies constitutional meaning without the friction of amendment. The executive has arbitrage (can choose other readings, can accept constraint of formal amendment) and chooses reinterpretation because it solves the coordination problem faster. Net beneficiary.
constraint_indexing:constraint_classification(article_9_renunciation__reinterpretation_2014_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: PACIFIST CONSTITUTIONAL IDENTITY (SNARE) — Agents whose political and national identity were constituted through the Article 9 renunciation face extraction of a core identity commitment. The reinterpretation is experienced not as policy change but as constitutional betrayal. Identity-locked exit: these agents structurally could (via politics) mount opposition to reverse the reinterpretation, but identity-fusion with the pacifist reading makes exit from the prior commitment unthinkable from within their frame. The extraction operates on identity, not just resources.
constraint_indexing:constraint_classification(article_9_renunciation__reinterpretation_2014_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(identity_locked),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From the civilizational/universal perspective, constitutional texts are immutable instruments — they have a fixed meaning discoverable by proper interpretation, and reinterpretation that contradicts prior readings is structurally impossible (the text means what it means). This perspective sees the reinterpretation as either a misreading or a violation of constitutional law itself. However, this classification is a false summit: the structural data reveals that the reinterpretation is a contingent institutional choice, not a natural law.
constraint_indexing:constraint_classification(article_9_renunciation__reinterpretation_2014_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_9_renunciation__reinterpretation_2014_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(article_9_renunciation__reinterpretation_2014_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(article_9_renunciation__reinterpretation_2014_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(article_9_renunciation__reinterpretation_2014_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(article_9_renunciation__reinterpretation_2014_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The reinterpretation extracts constitutional authority — it allows the executive to settle meaning without supermajority approval, saving the cost of a two-thirds amendment campaign. This is not maximal extraction because the executive's action does solve a genuine coordination problem (alliance deadlock, strategic ambiguity), and the reinterpretation does not impose arbitrary new obligations (collective defense is a rational response to threat environment). But the extraction IS present because the gate is crossed without payment. Suppression (0.72): High. Substantial barriers now exist to reversing the reinterpretation via formal amendment (requires new two-thirds supermajority to undo what cabinet has done) or via new reinterpretation (precedent now favors expansive executive reading power). The prior amendment path is suppressed — the Diet's supermajority authority is rendered symbolic. Theater ratio (0.68): Moderate-high. The cabinet decision is partly substantive (collective defense is operationally meaningful, alliance coordination is real) and partly performative (the assertion of executive constitutional authority, the theatrical legality-granting of military operations that were becoming necessary anyway). The theater increased from 0.35 to 0.68 across the interval because the executive's confidence in its meaning-settling role has grown, and the reinterpretation's legitimacy is sustained by institutional assertion rather than legal derivation.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits a deep perspectival chasm. The executive and alliance partners see pure coordination (Rope) — deadlock broken, alliance clarified. The formal amendment procedure sees pure extraction (Snare) — circumvented, rendered decorative. The Diet minority sees mixed extraction and coordination (Tangled Rope) — forced into deadlock-breaking cooperation while their constitutional authority is stripped. Identity-locked pacifists see extraction of their core commitment (Snare) — a constitutional betrayal. The natural law view sees an immutable text being reinterpreted (Mountain) — but this is a false summit because the structural data reveals executive agency, not unchangeable law. No perspective is wrong; each reads the same structural data from a different position in the extraction flow.
 *
 * DIRECTIONALITY LOGIC:
 *   The reinterpreting executive (institutional/arbitrage) derives a low d-value (0.15–0.20): it is a beneficiary with high exit options (could accept formal amendment instead). The amendment procedure gate (powerless/trapped) derives a high d-value (0.95): it is a victim with no exit. The security alliance (institutional/arbitrage) derives a low d-value (0.12): beneficiary with exit options. The Diet minority (moderate/constrained) derives a moderate d-value (0.60–0.65): victim status (power stripped) but some exit (can mount reversal campaign, can challenge constitutionality). Identity-locked pacifists (moderate/identity_locked) derive a high d-value (0.88–0.92): structurally they could mobilize politically, but identity fusion prevents them from perceiving exit from the pacifist commitment as anything other than existential loss. The chi values reflect these d-disparities: executive and alliance experience negative chi (beneficiaries), amendment gate experiences maximal chi (victim), diet minority experiences moderate chi, identity-locked agents experience high chi despite structural mobility.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy resolves through reading-identity rather than through single classification. This constraint IS a tangled rope — it mixes genuine coordination (alliance deadlock solved) with extractive bypassing of formal procedure (amendment gate suppressed). The snare readings (amendment gate, identity-locked pacifists) are correct for those positions. The rope readings (executive, alliance) are correct for those positions. The false summit (natural law immutability) reveals that constitutional texts acquire meaning through institutional practice, not discovery of eternal meaning. The mandatrophy is not 'which type is correct?' but 'which reading of Article 9 does the cabinet's reading instantiate?' This story answers: the reinterpretation reading — a reading that privileges coordination over procedure, executive meaning-settling over supermajority gates, alliance operability over pacifist identity-constitutive commitment.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    interpretation_vs_amendment_boundary,
    'Where is the structural boundary between legitimate constitutional interpretation and constitutive amendment? What makes a reinterpretation cross from one to the other?',
    'Comparative constitutional law analysis: how do courts in other democracies (US, Germany, Canada) distinguish interpretation from amendment? Does the degree of change (scope of affected rights), the suddenness (timeline of reversal), the deliberate evasion of amendment procedures (intent to bypass supermajority), or the contradiction with prior readings serve as the diagnostic boundary?',
    'If interpretation can legitimately reverse decades of practice: reinterpretation reading is valid, Article 9 is plastic, executive has latitude. If amendment is necessary when reversing foundational commitments: reinterpretation is extraction (suppresses amendment path), Article 9 is fixed, executive overreached.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpretation_vs_amendment_boundary, conceptual, 'Boundary between constitutional interpretation and amendment').

omega_variable(
    cabinet_authority_for_constitutional_meaning,
    'Does the constitution grant the cabinet (executive branch) the authority to authoritatively settle constitutional meaning, or is this authority reserved to the Diet (legislature) via amendment or to courts via judicial review?',
    'Textual analysis of the 1947 Constitution: Article 96 reserves amendment to the Diet; Article 81 grants courts judicial review power to invalidate unconstitutional laws. Does the constitution vest any meaning-clarification power in the executive? Examine the legal status of cabinet decisions (Cabinet Decision on Collective Self-Defense, July 2014) — are they binding on the Diet, or merely advisory?',
    'If cabinet has meaning-settling authority: reinterpretation is legitimate coordination. If only Diet and courts have this power: cabinet reinterpretation is ultra vires (beyond authority), and the constraint is extraction via usurpation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(cabinet_authority_for_constitutional_meaning, empirical, 'Constitutional authority for cabinet to settle meaning').

omega_variable(
    prior_interpretation_binding_force,
    'Did the 60+ year prior interpretation of Article 9 (Self-Defense Forces are constitutional, collective self-defense is not) acquire binding constitutional status through practice, creating a ''unamendable'' supraconstitutional norm? Or was it always revisable by later reinterpretation?',
    'Constitutional history: trace judicial decisions, legislative statements, and executive practice from 1947–2014. Document whether courts explicitly treated the collective-defense prohibition as settled or as an open question. Examine whether the prior interpretation was ever formally ratified by the Diet or courts, or merely acquiesced to.',
    'If prior interpretation was binding practice with supraconstitutional status: reinterpretation violates a deeper constitutional commitment and is extraction. If it was always revisable: reinterpretation is legitimate, and the constraint is pure coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(prior_interpretation_binding_force, empirical, 'Whether prior interpretation acquired binding supraconstitutional status').

omega_variable(
    reading_vs_sibling_foreclosure,
    'Does the 2014 reinterpretation foreclose the absolute-pacifism reading, or do both readings remain live constitutional options held by different political factions?',
    'Political/legal trajectory analysis: Can the Diet restore the pacifist reading via amendment (or new reinterpretation) without that action being viewed as unconstitutional? If yes, readings coexist; if no, one forecloses the other. Does the pacifist reading retain live advocates in the Diet, courts, and civil society who claim the reinterpretation is a legal mistake, not a settled question?',
    'If forecloses: the reinterpretation reading is the dominant constitutional voice, and the pacifist reading is now marginal or foreclosed. If coexists: both readings remain live constitutional claims, the reinterpretation is a victory for one faction but not a binding resolution, and the cycle could reverse.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_vs_sibling_foreclosure, conceptual, 'Whether reinterpretation forecloses or coexists with pacifism reading').

omega_variable(
    constitutional_revision_mandate,
    'Does the 2014 reinterpretation require formal Article 96 amendment to acquire full constitutional legitimacy (since it contradicts 60+ years of prior interpretation), or is cabinet reinterpretation sufficient?',
    'Normative constitutional theory: Compare the legitimacy cost of the reinterpretation as-is (executive fiat, no supermajority) vs. if formalized via Article 96 amendment. Which produces greater democratic legitimacy? Have any political parties or civil society movements demanded that the Diet formally amend Article 9 to codify collective self-defense?',
    'If amendment is normatively required: the reinterpretation reading is incomplete and unstable; the constraint persists until Article 96 is invoked. If cabinet reinterpretation suffices: the constraint is resolved by interpretation, amendment is bypassed, and the extraction is complete.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(constitutional_revision_mandate, preference, 'Whether Article 96 amendment is necessary for full constitutional legitimacy').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_9_renunciation__reinterpretation_2014_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(a9_2014_theater_t0_preinterpretation, article_9_renunciation__reinterpretation_2014_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(a9_2014_theater_t5_transition, article_9_renunciation__reinterpretation_2014_reading, theater_ratio, 5, 0.52).
narrative_ontology:measurement(a9_2014_theater_t10_post_reinterpretation, article_9_renunciation__reinterpretation_2014_reading, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(a9_2014_extractiveness_t0_preinterpretation, article_9_renunciation__reinterpretation_2014_reading, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(a9_2014_extractiveness_t5_transition, article_9_renunciation__reinterpretation_2014_reading, base_extractiveness, 5, 0.35).
narrative_ontology:measurement(a9_2014_extractiveness_t10_post_reinterpretation, article_9_renunciation__reinterpretation_2014_reading, base_extractiveness, 10, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(a9_2014_suppression_t0_preinterpretation, article_9_renunciation__reinterpretation_2014_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(a9_2014_suppression_t5_transition, article_9_renunciation__reinterpretation_2014_reading, suppression_requirement, 5, 0.48).
narrative_ontology:measurement(a9_2014_suppression_t10_post_reinterpretation, article_9_renunciation__reinterpretation_2014_reading, suppression_requirement, 10, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_9_renunciation__reinterpretation_2014_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(article_9_renunciation__reinterpretation_2014_reading, article_9_renunciation__absolute_pacifism_reading).
narrative_ontology:affects_constraint(article_9_renunciation__reinterpretation_2014_reading, article_9_renunciation__self_defense_interpretation_reading).

% DUAL FORMULATION NOTE:
% The Article 9 kernel decomposes into three distinct constraint stories, one per reading. Each reading has its own extractiveness, suppression, and classification because each reading instantiates a different structural relationship to the constitutional text and the amendment procedure. The reinterpretation_2014_reading (this story) treats interpretation as a legitimate mode of constitutional change; the absolute_pacifism_reading treats reinterpretation as textual violation; the self_defense_interpretation_reading treats the text as already permitting defense, rendering reinterpretation unnecessary. All three stories are linked via network.affects_constraints because they compete for the same kernel's meaning and the same constitutional authority.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(article_9_renunciation__reinterpretation_2014_reading, institutional, 0.18).
constraint_indexing:directionality_override(article_9_renunciation__reinterpretation_2014_reading, moderate, 0.62).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
