% ============================================================================
% CONSTRAINT STORY: article_9_war_renunciation__collective_self_defense_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_9_war_renunciation__collective_self_defense_reading, []).

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
 *   constraint_id: article_9_war_renunciation__collective_self_defense_reading
 *   human_readable: Article 9 Collective Self-Defense Interpretation (Survival-Threat Doctrine)
 *   domain: constitutional/security policy
 *
 * SUMMARY:
 *   Japan's Article 9 constitutional commitment to war renunciation has been
 *   subject to competing interpretations since 1947. This constraint story
 *   instantiates the COLLECTIVE SELF-DEFENSE READING, which holds that Japan
 *   retains an inherent right to collective defense of allies when Japan's
 *   own survival is threatened, even without direct attack on Japanese
 *   territory. Under this reading, overseas deployments and joint operations
 *   with allies become permissible whenever policymakers invoke a 'survival
 *   threat' trigger. The reading conflicts with two sibling readings: the
 *   STRICT PACIFIST reading (no armed forces at all, ever) and the INHERENT
 *   RIGHT reading (self-defense only, narrowly construed to defensive
 *   capacity at home). This story is ONE of three instantiations of the
 *   contested Article 9 kernel; the sibling readings are separate constraint
 *   stories, each with its own ε and metrics.
 *
 * KEY AGENTS:
 *   - Executive branch defense actors: Defense Ministry and political leadership setting doctrine, administering the collective self-defense interpretation, enabling overseas operations
 *   - Allied security apparatus (primarily U.S.): Institutional beneficiary of Japanese operational flexibility; gains without bearing constitutional-legitimacy cost
 *   - Strict pacifist constituency: Identity-locked opposition; constitution-committed to absolute prohibition; power moderate; exit costly because identity fused to Article 9 text
 *   - Regional partners threatened by expansion (China, Russia): Powerful but constrained; bear rising security costs from Japanese power-projection expansion; excluded from reinterpretation process
 *   - Constitutional court: Dual role—agenda setter (interprets text) and observer (maintains deferential judicial review); enables the constraint's persistence
 *   - Legislative pacifist minority: Excluded from power despite holding formal legislative seat; cannot muster votes to legislatively reverse doctrine
 *   - Historical reparations coalition: Powerless, trapped, regional; excluded from Japanese law; appeals to Article 9's original restraint fall on actors with no institutional incentive to listen
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_9_war_renunciation__collective_self_defense_reading, 0.68).
domain_priors:suppression_score(article_9_war_renunciation__collective_self_defense_reading, 0.61).
domain_priors:theater_ratio(article_9_war_renunciation__collective_self_defense_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_9_war_renunciation__collective_self_defense_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(article_9_war_renunciation__collective_self_defense_reading, suppression_requirement, 0.61).
narrative_ontology:constraint_metric(article_9_war_renunciation__collective_self_defense_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_9_war_renunciation__collective_self_defense_reading, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(article_9_war_renunciation__collective_self_defense_reading, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_9_war_renunciation__collective_self_defense_reading, tangled_rope).
narrative_ontology:human_readable(article_9_war_renunciation__collective_self_defense_reading, "Article 9 Collective Self-Defense Interpretation (Survival-Threat Doctrine)").
narrative_ontology:topic_domain(article_9_war_renunciation__collective_self_defense_reading, "constitutional/security policy").

domain_priors:requires_active_enforcement(article_9_war_renunciation__collective_self_defense_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_9_war_renunciation__collective_self_defense_reading, 'ee425817-d493-4f67-a03d-4627f45c903f').
narrative_ontology:cs_kernel_codification('ee425817-d493-4f67-a03d-4627f45c903f', fixed_text).
narrative_ontology:cs_authority_grounding('ee425817-d493-4f67-a03d-4627f45c903f', lineage).
narrative_ontology:cs_interpretation_layer_present('ee425817-d493-4f67-a03d-4627f45c903f').
narrative_ontology:cs_reading_relation('ee425817-d493-4f67-a03d-4627f45c903f', article_9_war_renunciation__strict_pacifist_reading, coexists_with).
narrative_ontology:cs_reading_relation('ee425817-d493-4f67-a03d-4627f45c903f', article_9_war_renunciation__inherent_right_reading, influences).
narrative_ontology:cs_axiom('ee425817-d493-4f67-a03d-4627f45c903f', foundational, self_defense_right_is_inherent).
narrative_ontology:cs_axiom_status(self_defense_right_is_inherent, holdable).
narrative_ontology:cs_axiom_grounding('ee425817-d493-4f67-a03d-4627f45c903f', self_defense_right_is_inherent, deontological).
narrative_ontology:cs_axiom('ee425817-d493-4f67-a03d-4627f45c903f', secondary, collective_defense_trigger_is_survival_threat).
narrative_ontology:cs_axiom_status(collective_defense_trigger_is_survival_threat, holdable).
narrative_ontology:cs_axiom_grounding('ee425817-d493-4f67-a03d-4627f45c903f', collective_defense_trigger_is_survival_threat, empirically_contingent).
narrative_ontology:cs_reference_frame('ee425817-d493-4f67-a03d-4627f45c903f', absolute_war_renunciation).
narrative_ontology:cs_drift_state('ee425817-d493-4f67-a03d-4627f45c903f', contemporary_post_2014, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('ee425817-d493-4f67-a03d-4627f45c903f', '').
narrative_ontology:cs_kernel_id(article_9_war_renunciation__collective_self_defense_reading, article_9_war_renunciation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__collective_self_defense_reading, executive_branch_defense_actors).
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__collective_self_defense_reading, allied_security_apparatus).
narrative_ontology:constraint_victim(article_9_war_renunciation__collective_self_defense_reading, strict_pacifist_constituency).
narrative_ontology:constraint_victim(article_9_war_renunciation__collective_self_defense_reading, regional_partners_threatened_by_expansion).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The Defense Ministry and political leadership that interpret Article 9's scope. They claim the inherent right to self-defense permits overseas joint operations when Japan's 'survival' is threatened—a trigger that has become increasingly elastic. They set doctrine, approve deployments, and defend the interpretation against constitutional challenge. They benefit from interpretive latitude that permits power-projection without formal constitutional amendment.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__collective_self_defense_reading, executive_branch_defense_actors, agenda_setter,
    institutional, generational, arbitrage, regional).

% Partner nations (primarily the United States) that gain from Japanese military participation in regional operations under the collective self-defense reading. Their security commitment to Japan becomes more operationally flexible when Japan can deploy globally under a self-defense rationale. They benefit from the interpretive expansion without bearing the constitutional-legitimacy cost.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__collective_self_defense_reading, allied_security_apparatus, beneficiary,
    institutional, generational, constrained, regional).

% Constitutional scholars, civil society groups, and citizens committed to Article 9's original absolute prohibition. They bear the cost of watching their foundational constitutional commitment reinterpreted away through judicial tolerance and executive doctrinal creep. Their identity and political legitimacy rest partly on Article 9 as written; each expansive interpretation diminishes their framework's standing. Exit through relocation is costly; political exit (building a majority to reverse the doctrine) faces institutional entrenchment.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__collective_self_defense_reading, strict_pacifist_constituency, payer,
    moderate, biographical, identity_locked, national).

% Nations in the region (e.g., China, Russia) that interpret Japan's operational expansion under the 'survival threat' doctrine as arms buildup and destabilization. They expected Article 9 to constrain Japanese power; the reinterpretation narrows their strategic margin and forces countervailing military investments. Their own security costs rise without direct agency in the reinterpretation.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__collective_self_defense_reading, regional_partners_threatened_by_expansion, payer,
    powerful, generational, constrained, regional).

% The judiciary that has, in practice, declined to strike down executive reinterpretations of Article 9 as unconstitutional overreach. They maintain judicial review authority but exercise it deferentially toward security and political judgment. They both administer and enable the interpretive constraint.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__collective_self_defense_reading, constitutional_court, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(article_9_war_renunciation__collective_self_defense_reading, constitutional_court, observer).

% Diet members and parties that oppose the collective self-defense reading and seek legislation or constitutional amendment to restore the absolute prohibition. They lack the votes to change the law directly and are structurally excluded from enforcing their reading despite holding formal legislative seat.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__collective_self_defense_reading, legislative_pacifist_minority, excluded,
    moderate, biographical, constrained, national).

% Civil society in formerly occupied territories and neighboring states that see Article 9's original absolutism as Japan's binding postwar commitment to restraint. They are not seated in Japanese law and cannot constrain the reinterpretation except through diplomatic pressure, which has proven ineffectual.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__collective_self_defense_reading, historical_reparations_coalition, excluded,
    powerless, biographical, trapped, regional).

% Constitutional law scholars, policy analysts, and international observers who track how written law becomes interpretively elastic over time. They see this constraint as a live experiment in how textual prohibitions survive when executive actors find functional loopholes and courts decline to enforce the text.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__collective_self_defense_reading, analytical_observer, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(article_9_war_renunciation__collective_self_defense_reading, executive_branch_defense_actors).
narrative_ontology:fixing_cost_class(article_9_war_renunciation__collective_self_defense_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables Japan and allied nations to coordinate military response to regional threats that do not directly attack Japanese territory but are framed as threatening Japan's 'survival'—narrowing the response-lag problem in distributed security architecture and enabling faster joint action.
% TRANSFER_FUNCTION: Transfers operational latitude from legislative and strict constitutional constraint into executive hands (and by extension, into allied command structures). It moves decision-making authority from explicit Article 9 text into reinterpretable doctrine, and it transfers strategic freedom from pacifist stakeholders to defense actors.
% ABSENT_VOICES: Citizens across the region who expect Article 9 to constrain Japanese military reach (historical reparations coalitions, regional publics) are structurally excluded from the reinterpretation process. Legislative pacifist minority holds formal seat but lacks power to contest the judicial-executive consensus.
% DISAPPEARANCE_RATIONALE: If the collective self-defense reading vanished and Article 9's text reverted to strict interpretation, Japanese military doctrine would contract sharply—overseas deployments would face legal jeopardy, joint operations would require legislative approval for each case, and regional security architecture built on Japanese operational availability would require restructuring. Allied defense planning would shift to reduce reliance on Japanese power projection.
% FOUNDING_PROBLEM: Post-1945 Japan needed to participate in regional security without violating the occupation-imposed absolute war renunciation (Article 9). Self-defense was always allowed; the doctrinal question became how far self-defense could extend. The problem was framed as: How can a pacifist constitution survive in a region of rising threats?
% FOUNDING_PROBLEM_CORROBORATION: Defense actors and security strategists attest the founding problem is live and requires the collective self-defense reading for Japan to meet its regional commitments. Pacifist scholars and civil society attest the problem was a post-war fiction—that absolute pacifism WAS the foundational commitment and the 'problem' is a manufactured conflict invented by actors seeking to evade the constraint. International observers corroborate the contested status: some read Japan's regional role as requiring flexibility, others read the same role as a breach of the original constitutional settlement.
narrative_ontology:disappearance_verdict(article_9_war_renunciation__collective_self_defense_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_9_war_renunciation__collective_self_defense_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_9_war_renunciation__collective_self_defense_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(article_9_war_renunciation__collective_self_defense_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_9_war_renunciation__collective_self_defense_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_9_war_renunciation__collective_self_defense_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(article_9_war_renunciation__collective_self_defense_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(article_9_war_renunciation__collective_self_defense_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   This reading is CLAIMED AS TANGLED ROPE because it simultaneously coordinates (enables regional security response) and extracts (transfers decision authority from strict text to executive reinterpretation, benefits allies without constitutional cost to them, harms pacifist stakeholders). Extractiveness is high (0.68 at interval end, rising from 0.42 over the measurement interval) because the 'survival threat' trigger is elastic—it can be invoked for operations far removed from direct defense and has progressively widened without textual amendment. Suppression is significant (0.61 because strict pacifist voices must be managed through judicial deference and media frames rather than textual law; pacifist readings are not silenced but are institutionally constrained). Theater is moderate (0.42: the security-coordination function is real, but a growing share of doctrine-maintenance is defensive—defending the interpretation against textual objections—rather than solving novel security problems). The measurement series shows EXTRACTION ACCUMULATION: ε rises from 0.42 to 0.68 over the interval as incremental deployments and doctrinal creep compound, while suppression plateaus around 0.61 (the judicial-executive consensus stabilizes). This pattern is classic tangled-rope drift: the coordination benefit (present throughout) remains constant, but extraction layered on top of it grows as the doctrine becomes more elastic.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (executive defense actors) and beneficiary seat (allies) experience the constraint as a workable coordination mechanism—necessary flexibility to meet regional security challenges. The payer seats (strict pacifist constituency, regional rivals) experience it as enforced reinterpretation that violates a foundational commitment. The court occupies a hinge: it is formally the authority on constitutional meaning but has exercised its role deferentially, validating executive readings rather than enforcing textual constraint. From the pacifist seat, the court's passivity is a failure of law; from the executive seat, it is wisdom (acknowledging legitimate security needs). The engine should compute these seats differently: the executive and allies should show low effective extraction (they perceive coordination benefit); pacifist and regional seats should show high extraction (they see constraint violation). The ε value (0.68) is authored at the reading's own lights (as the constraint-on-text interprets it) and is reading-indexed: a strict pacifist reading of the same kernel would author far lower ε for the 'absolute prohibition' constraint, because it would measure against the absolute text. This ε is for the collective-self-defense reading's own constraint instantiation—the standing arrangement it defends.
 *
 * DIRECTIONALITY LOGIC:
 *   The 'survival threat' trigger is the critical structural feature. It is elastic: it can be invoked without direct attack on Japanese territory (hence 'collective' rather than unilateral), and what counts as a threat to survival is left to executive judgment. This elasticity is the enforcement mechanism—it keeps alternatives collapsed (what counts as legal action is determined by the executive's reading, not the text) and suppresses opposition (pacifist readings are not forbidden but are rendered constitutionally illegitimate by judicial tolerance of the executive interpretation). The identity-lock on the pacifist constituency is crucial: they cannot exit because their political identity is constituted by Article 9 absolutism. A regional rival can diversify military posture or seek different alliances; a pacifist cannot un-read the constitution or find a different nation where Article 9 means what they thought it meant. That asymmetry drives the directionality divergence.
 *
 * MANDATROPHY ANALYSIS:
 *   The collective self-defense reading invokes a founding problem (regional security requires flexibility) that is itself contestable. The strict pacifist reading invokes a founding problem (war renunciation is absolute) that is backed by the text and the occupation history. The mandate question is: Which founding problem is REAL? The collective-self-defense reading answers: both exist, but security pragmatism overrides textual absolutism when survival is threatened. That trade-off is a TANGLED ROPE justification—it admits coordination benefit (security) and extraction cost (constitutional erosion) coexist and require active enforcement (judicial deference) to maintain. If the mandate were dead (if the founding problem no longer existed), the constraint would become pure extraction and would be reclassified as a SNARE. Evidence of a dead mandate: if regional security threats subsided but the doctrine remained, if the doctrine were invoked for non-survival operations (e.g., alliance signaling with no real security content), or if allies' security improved while pacifist constraint eroded further. The measurement series shows extraction accumulation (ε rising while suppression plateaus), which is consistent with the mandate remaining live (security justifies expansion) but being stretched (the 'survival' trigger is invoked for increasingly marginal threats). This is the classic TANGLED ROPE lifecycle: the coordination function remains real, but it is being used to cover incremental extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    survival_threat_elasticity,
    'What counts as a threat to Japan''s ''survival''? Is the trigger definitionally linked to direct threats to Japanese territory and population, or can it encompass any threat to Japan''s regional role and alliance relationships?',
    'Document the executive''s successive invocations of the survival trigger over time and classify by proximity to Japan: does the trigger require imminent threat to Japanese soil, or is it applied to threats hundreds of miles away? If applied to distant threats framed as destabilizing the region, the elasticity is high.',
    'If survival is elastic (can mean regional stability, alliance credibility, power-balance preservation), the constraint becomes more extractive—the executive gains broad authority to deploy without direct defense rationale. If survival is rigid (imminent threat only), the constraint remains more coordinative and less extractive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(survival_threat_elasticity, conceptual, 'The semantic scope of ''survival threat'' determines the constraint''s true extractiveness.').

omega_variable(
    kernel_reading_foreclosure,
    'Do the COLLECTIVE SELF-DEFENSE and STRICT PACIFIST readings logically foreclose each other, or do they simply occupy different parties'' commitments simultaneously?',
    'Examine whether a party could coherently hold both readings or whether accepting one requires denying the core premise of the other. Can a court say ''the text forbids all war AND permits collective self-defense when survival is threatened''?',
    'If foreclosed: this reading''s success in courts implies the pacifist reading must be formally rejected. If coexistent: both readings remain live options for different parties, and the constraint''s persistence depends on institutional power, not logical necessity. Coexistence is the working reality; foreclosure would indicate the pacifist reading has been definitively superseded.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure, conceptual, 'Whether the reading relationship is logical foreclosure or institutional coexistence.').

omega_variable(
    mandate_obsolescence,
    'Has the founding problem (regional security threats require collective response) changed materially? If threats subsided but the doctrine persisted, would it become a zombie constraint (dead mandate, persistent extraction)?',
    'Track regional threat assessments by defense analysts and allied partners over time. If threat perceptions decline while deployment authority remains, the mandate has become historical cover for extraction.',
    'If mandate is dead: the constraint should reclassify from tangled_rope to snare (pure extraction with coordination cover). If mandate is live but being stretched: the constraint remains tangled_rope but theater ratio should rise as more work goes into justifying marginal deployments.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(mandate_obsolescence, empirical, 'Whether the collective-self-defense doctrine''s founding security problem remains live or has become historical cover.').

omega_variable(
    judicial_deference_mechanism,
    'Why does the constitutional court defer to executive interpretation of Article 9? Is it because the security judgment is genuinely technical and non-justiciable, or because courts lack the political will to enforce textual constraint against executive power?',
    'Compare judicial review intensity in security cases vs. other constitutional domains. If courts apply deferential review ONLY to security, the deference is political rather than principled (the mechanism is suppression). If courts apply similar deference across domains, the mechanism may be institutional respect for executive competence.',
    'If political: the court is part of the enforcement machinery keeping the reading dominant, and suppression is structural (institutional bias toward executive). If principled: the deference is a legitimate division of labor, and suppression is lower (the reading has passed a neutral institutional test).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_deference_mechanism, conceptual, 'Whether judicial deference to executive security interpretation is principled or political.').

omega_variable(
    reading_identity_fusion_thesis,
    'Is the pacifist constituency''s opposition to collective self-defense primarily a constitutional position (the text doesn''t permit it) or an identity position (we are the nation that renounced war, and this reading unmakes that identity)?',
    'Survey pacifist opposition on grounds cited: are objections framed as ''the text forbids this'' (constitutional) or ''this betrays what we are'' (identity)? If identity-framed, opposition is internalized and harder to overcome through argumentation alone.',
    'If primarily constitutional: opposition could shift if the text were reworded or courts clearly validated the reading. If primarily identity: opposition is locked in regardless of judicial validation, because accepting the reading requires self-erasure. Identity fusion increases effective suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_identity_fusion_thesis, empirical, 'Whether pacifist opposition is constitutional position or fused-identity opposition.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_9_war_renunciation__collective_self_defense_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t0, article_9_war_renunciation__collective_self_defense_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(arti_tr_t5, article_9_war_renunciation__collective_self_defense_reading, theater_ratio, 5, 0.29).
narrative_ontology:measurement(arti_tr_t10, article_9_war_renunciation__collective_self_defense_reading, theater_ratio, 10, 0.33).
narrative_ontology:measurement(arti_tr_t15, article_9_war_renunciation__collective_self_defense_reading, theater_ratio, 15, 0.37).
narrative_ontology:measurement(arti_tr_t20, article_9_war_renunciation__collective_self_defense_reading, theater_ratio, 20, 0.39).
narrative_ontology:measurement(arti_tr_t25, article_9_war_renunciation__collective_self_defense_reading, theater_ratio, 25, 0.41).
narrative_ontology:measurement(arti_tr_t30, article_9_war_renunciation__collective_self_defense_reading, theater_ratio, 30, 0.42).
narrative_ontology:measurement(arti_tr_t35, article_9_war_renunciation__collective_self_defense_reading, theater_ratio, 35, 0.42).
narrative_ontology:measurement(arti_tr_t40, article_9_war_renunciation__collective_self_defense_reading, theater_ratio, 40, 0.42).

% Extraction over time
narrative_ontology:measurement(arti_be_t0, article_9_war_renunciation__collective_self_defense_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(arti_be_t5, article_9_war_renunciation__collective_self_defense_reading, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(arti_be_t10, article_9_war_renunciation__collective_self_defense_reading, base_extractiveness, 10, 0.54).
narrative_ontology:measurement(arti_be_t15, article_9_war_renunciation__collective_self_defense_reading, base_extractiveness, 15, 0.6).
narrative_ontology:measurement(arti_be_t20, article_9_war_renunciation__collective_self_defense_reading, base_extractiveness, 20, 0.64).
narrative_ontology:measurement(arti_be_t25, article_9_war_renunciation__collective_self_defense_reading, base_extractiveness, 25, 0.66).
narrative_ontology:measurement(arti_be_t30, article_9_war_renunciation__collective_self_defense_reading, base_extractiveness, 30, 0.67).
narrative_ontology:measurement(arti_be_t35, article_9_war_renunciation__collective_self_defense_reading, base_extractiveness, 35, 0.68).
narrative_ontology:measurement(arti_be_t40, article_9_war_renunciation__collective_self_defense_reading, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t0, article_9_war_renunciation__collective_self_defense_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(arti_su_t5, article_9_war_renunciation__collective_self_defense_reading, suppression_requirement, 5, 0.43).
narrative_ontology:measurement(arti_su_t10, article_9_war_renunciation__collective_self_defense_reading, suppression_requirement, 10, 0.49).
narrative_ontology:measurement(arti_su_t15, article_9_war_renunciation__collective_self_defense_reading, suppression_requirement, 15, 0.54).
narrative_ontology:measurement(arti_su_t20, article_9_war_renunciation__collective_self_defense_reading, suppression_requirement, 20, 0.58).
narrative_ontology:measurement(arti_su_t25, article_9_war_renunciation__collective_self_defense_reading, suppression_requirement, 25, 0.6).
narrative_ontology:measurement(arti_su_t30, article_9_war_renunciation__collective_self_defense_reading, suppression_requirement, 30, 0.61).
narrative_ontology:measurement(arti_su_t35, article_9_war_renunciation__collective_self_defense_reading, suppression_requirement, 35, 0.61).
narrative_ontology:measurement(arti_su_t40, article_9_war_renunciation__collective_self_defense_reading, suppression_requirement, 40, 0.61).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_9_war_renunciation__collective_self_defense_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(article_9_war_renunciation__collective_self_defense_reading, 0.12).
narrative_ontology:affects_constraint(article_9_war_renunciation__collective_self_defense_reading, article_9_war_renunciation__inherent_right_reading).
narrative_ontology:affects_constraint(article_9_war_renunciation__collective_self_defense_reading, article_9_war_renunciation__strict_pacifist_reading).

% DUAL FORMULATION NOTE:
% This story is one reading of a three-way kernel contest over Article 9's meaning. The COLLECTIVE SELF-DEFENSE reading claims that inherent self-defense rights extend to joint operations when Japan's survival is threatened. It is downstream of and influences both the INHERENT RIGHT reading (which permits narrower self-defense) and the STRICT PACIFIST reading (which forbids armed force entirely). The three readings are separate constraints with distinct ε values, beneficiary/victim structures, and classifications. Collective self-defense is tangled_rope (coordination + extraction); inherent right is rope (pure coordination); strict pacifist is mountain (natural law). They share the kernel (Article 9 text) but instantiate different commitments about what the text permits. The family relationship is: strict_pacifist FORECLOSES inherent_right (if the text forbids all force, it forbids defense); inherent_right INFLUENCES collective_self_defense (establishing that self-defense is permissible makes the question of collective extension an incremental step); collective_self_defense COEXISTS_WITH both others at the level of institutional parties (the court, the executive, and pacifist stakeholders each hold a reading simultaneously).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
