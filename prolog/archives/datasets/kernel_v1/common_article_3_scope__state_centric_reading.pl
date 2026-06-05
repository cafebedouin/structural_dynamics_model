% ============================================================================
% CONSTRAINT STORY: common_article_3_scope__state_centric_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_common_article_3_scope__state_centric_reading, []).

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
 *   constraint_id: common_article_3_scope__state_centric_reading
 *   human_readable: Common Article 3 Scope: State-Centric Reading (Intensity/Organization Thresholds)
 *   domain: international_humanitarian_law/law_of_armed_conflict
 *
 * SUMMARY:
 *   Common Article 3 of the 1949 Geneva Conventions establishes baseline
 *   humanitarian protections in conflicts 'not of an international
 *   character.' The state-centric reading interprets CA3 scope through
 *   intensity and organization thresholds: the constraint applies only when
 *   armed conflict reaches a minimum level of organization and sustained
 *   intensity, explicitly excluding low-level violence, law enforcement
 *   operations, and sporadic armed activity. This reading creates a
 *   structural arrangement where governments retain maximum discretion to
 *   classify conflicts as 'internal security' or 'below threshold,' thereby
 *   excluding irregular combatants and civilian populations from humanitarian
 *   protection. The reading coexists in contested space with the expansive
 *   human rights reading (CA3 applies to all organized armed violence as a
 *   humanitarian floor) and the ICRC customary reading (CA3 scope evolves
 *   through state practice and opinio juris). This constraint story
 *   instantiates the state-centric reading as a distinct, clean
 *   classification: it is the reading that privileges state operational
 *   freedom and defines the threshold mechanism as objective and neutral
 *   rather than as contestable institutional power.
 *
 * KEY AGENTS:
 *   - State Military and Executive Authority: Primary beneficiary (institutional/arbitrage) — retains definitional power over CA3 scope; captures operational freedom by classifying conflicts as 'below threshold'
 *   - Irregular Combatants Below Threshold: Primary victims (powerless/trapped) — excluded from CA3 protection by state classification; cannot appeal to humanitarian law; bears maximum suppression
 *   - Civilian Populations in Gray-Zone Conflicts: Secondary victims (moderate/constrained) — experience ambiguous legal status; coordination benefits from baseline protections mixed with extraction costs from state discretion
 *   - Humanitarian Agencies and Monitoring Organizations: Organized agents (organized/constrained) — coordinate with states on access and monitoring; constrained by state threshold definitions; limited power to challenge classifications
 *   - IHL Institutional Infrastructure (Courts, Treaty Bodies, Peacekeeping): Institutional actors (institutional/arbitrage) — maintain threshold gate-keeping through performative neutrality; inertial authority without enforcement capacity
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing the state-centric threshold as objective criteria inherent to 'armed conflict' rather than recognizing it as an institutional reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(common_article_3_scope__state_centric_reading, 0.58).
domain_priors:suppression_score(common_article_3_scope__state_centric_reading, 0.68).
domain_priors:theater_ratio(common_article_3_scope__state_centric_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(common_article_3_scope__state_centric_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(common_article_3_scope__state_centric_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(common_article_3_scope__state_centric_reading, theater_ratio, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(common_article_3_scope__state_centric_reading, snare).
narrative_ontology:human_readable(common_article_3_scope__state_centric_reading, "Common Article 3 Scope: State-Centric Reading (Intensity/Organization Thresholds)").
narrative_ontology:topic_domain(common_article_3_scope__state_centric_reading, "international_humanitarian_law/law_of_armed_conflict").

domain_priors:requires_active_enforcement(common_article_3_scope__state_centric_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(common_article_3_scope__state_centric_reading, 'a593dde1-1eb3-4ef0-9e8d-66808821a166').
narrative_ontology:cs_kernel_codification('a593dde1-1eb3-4ef0-9e8d-66808821a166', formalized).
narrative_ontology:cs_authority_grounding('a593dde1-1eb3-4ef0-9e8d-66808821a166', extraction).
narrative_ontology:cs_interpretation_layer_present('a593dde1-1eb3-4ef0-9e8d-66808821a166').
narrative_ontology:cs_reading_relation('a593dde1-1eb3-4ef0-9e8d-66808821a166', common_article_3_scope__expansive_human_rights_reading, coexists_with).
narrative_ontology:cs_reading_relation('a593dde1-1eb3-4ef0-9e8d-66808821a166', common_article_3_scope__icrc_customary_reading, influences).
narrative_ontology:cs_axiom('a593dde1-1eb3-4ef0-9e8d-66808821a166', foundational, state_discretion_threshold_definition).
narrative_ontology:cs_axiom_status(state_discretion_threshold_definition, holdable).
narrative_ontology:cs_axiom_grounding('a593dde1-1eb3-4ef0-9e8d-66808821a166', state_discretion_threshold_definition, deontological).
narrative_ontology:cs_axiom('a593dde1-1eb3-4ef0-9e8d-66808821a166', secondary, objectivity_of_threshold_criteria).
narrative_ontology:cs_axiom_status(objectivity_of_threshold_criteria, holdable).
narrative_ontology:cs_axiom_grounding('a593dde1-1eb3-4ef0-9e8d-66808821a166', objectivity_of_threshold_criteria, empirically_contingent).
narrative_ontology:cs_reference_frame('a593dde1-1eb3-4ef0-9e8d-66808821a166', state_sovereign_threshold_definition).
narrative_ontology:cs_drift_state('a593dde1-1eb3-4ef0-9e8d-66808821a166', contemporary_proliferation_of_gray_zone_conflict, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a593dde1-1eb3-4ef0-9e8d-66808821a166', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(common_article_3_scope__state_centric_reading, common_article_3_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(common_article_3_scope__state_centric_reading, state_military_apparatus).
narrative_ontology:constraint_beneficiary(common_article_3_scope__state_centric_reading, state_executive_discretion).
narrative_ontology:constraint_victim(common_article_3_scope__state_centric_reading, irregular_combatants_below_threshold).
narrative_ontology:constraint_victim(common_article_3_scope__state_centric_reading, civilian_populations_in_gray_zone_conflicts).
narrative_ontology:constraint_victim(common_article_3_scope__state_centric_reading, humanitarian_agencies_access).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: IRREGULAR COMBATANTS BELOW THRESHOLD (SNARE) — Non-state armed groups, militias, and organized resistance movements that do not meet the 'intensity and organization' threshold are excluded from CA3 protection. These actors face maximum suppression: no legal status, no humanitarian immunity, no prisoner-of-war protections. Cannot exit the conflict or appeal to CA3 without meeting state-defined criteria. Bears full extraction cost — vulnerability to unrestricted force while state retains operational discretion.
constraint_indexing:constraint_classification(common_article_3_scope__state_centric_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: CIVILIAN POPULATIONS IN GRAY-ZONE CONFLICTS (TANGLED ROPE) — Civilians in counterinsurgency, counter-terrorism, and internal security operations exist in a coordination-extraction hybrid. States coordinate security with humanitarian minimum protections (some CA3 application) but also extract operational freedom by excluding low-intensity conflicts from full CA3 scope. Civilians benefit from some baseline protections but suffer from the ambiguity — classification decisions are made by the state, not civilian protection advocates.
constraint_indexing:constraint_classification(common_article_3_scope__state_centric_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: STATE MILITARY AND EXECUTIVE AUTHORITY (ROPE) — Governments experience the state-centric reading as a coordination mechanism: intensity and organization thresholds enable military efficiency by clarifying when CA3 applies and when operational freedom is retained. States benefit from the threshold structure by preserving tactical flexibility in counterterrorism and internal security operations. Effective arbitrage: the state defines what counts as 'armed conflict' for CA3 purposes.
constraint_indexing:constraint_classification(common_article_3_scope__state_centric_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: IHL INSTITUTIONAL INFRASTRUCTURE (PITON) — The legal institutions tasked with interpreting and applying CA3 (national courts, treaty bodies, peacekeeping missions) maintain the intensity/organization threshold as a performative gate-keeping mechanism. The threshold is described as objective and neutral but functions as institutional theater: states operationalize it through ad hoc determinations that preserve executive discretion. The institutional infrastructure sees its own authority as degraded — it cannot enforce CA3 scope against state determinations — but persists through inertia and legitimacy claims.
constraint_indexing:constraint_classification(common_article_3_scope__state_centric_reading, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: HUMANITARIAN AGENCIES AND MONITORING ORGS (TANGLED ROPE) — Red Cross/Red Crescent, Human Rights Watch, Amnesty International, and UN fact-finding missions are constrained by state-defined CA3 scope. They coordinate with states to monitor compliance (coordination function) but extract the enforcement cost: restricted access to gray-zone conflict zones, exclusion from threshold-determination processes, and limited power to challenge state classifications. Organized agents with constrained exit — high cost to refusing a state's scope determination.
constraint_indexing:constraint_classification(common_article_3_scope__state_centric_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, the state-centric reading presents the intensity and organization thresholds as natural, objective criteria inherent to the concept of 'armed conflict' itself. The framing naturalizes what is actually a contestable institutional arrangement designed to preserve state discretion. The engine's false-summit detector will identify this as a naturalization of a contingent reading, revealing that the 'objective criteria' are actually epistemic claims that benefit state actors.
constraint_indexing:constraint_classification(common_article_3_scope__state_centric_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(common_article_3_scope__state_centric_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(common_article_3_scope__state_centric_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(common_article_3_scope__state_centric_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(common_article_3_scope__state_centric_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(common_article_3_scope__state_centric_reading, TR),
    TR >= 0.70.

:- end_tests(common_article_3_scope__state_centric_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The state-centric reading confers substantial benefits on governments while imposing significant costs on irregular combatants and gray-zone civilian populations. The benefit is primarily discretionary power (freedom to classify conflicts as below-threshold and retain operational freedom); the cost is humanitarian vulnerability and exclusion from legal protection. Extractiveness rises over time (0.42 → 0.58) as states increasingly adopt gray-zone conflict strategies (counterinsurgency, counter-terrorism) that exploit the threshold, suggesting the constraint is becoming a more refined extraction mechanism. Suppression (0.68): High. Irregular combatants and gray-zone populations have minimal alternatives to state-defined protection. The intensity/organization threshold removes the option to claim CA3 protection without meeting state-determined criteria. Suppression rises over time (0.55 → 0.68) as states develop increasingly sophisticated gray-zone operations that deliberately remain below the threshold, indicating suppression is actively reinforced through strategic behavior. Theater ratio (0.52): Moderate. The threshold mechanism is presented as objective and technical (based on conflict characteristics) but functions partly as institutional theater: states determine what counts as 'intensity and organization,' and the determination reflects political interest more than neutral criteria. Theater ratio rises gradually (0.48 → 0.52) as institutional practice accumulates more ad hoc threshold determinations that lack consistent logic, suggesting the mask of objectivity is thinning.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits a stark perspectival gap. States and their militaries see the threshold as a coordination mechanism (Rope) — a shared understanding of when humanitarian law applies. Humanitarian agencies see mixed coordination and extraction (Tangled Rope) — they coordinate access with states but accept state definitional power. Irregular combatants see pure extraction (Snare) — no negotiable thresholds, no protection below the state-defined line. The analytical observer risks seeing a natural law (Mountain) — the intensity and organization thresholds as inherent to the concept of armed conflict — but the structural data reveals this as a false summit: the thresholds are epistemic claims that benefit state actors and can be contested (as the expansive and customary-law readings demonstrate). The perspectival gap is the gap between state beneficiaries (who experience rope-like coordination) and non-state victims (who experience snare-like extraction).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from structural position within the constraint. State military apparatus: beneficiary status (controls threshold definition) + arbitrage exit (can reclassify conflicts as needed) → low d → low/negative χ → sees Rope. Irregular combatants: victim status (excluded from protection) + trapped exit (cannot appeal to alternative legal regime) → high d → high χ → sees Snare. Humanitarian agencies: organized actor status (have institutional voice) + constrained exit (must negotiate with states) → moderate d → moderate χ → sees Tangled Rope. The engine's computation of directionality from beneficiary/victim + exit options will produce this perspectival structure automatically.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint avoids false classification between coordination and extraction by explicitly anchoring beneficiary/victim declarations. The state-centric reading is not a simple rope (coordination on conflict thresholds) because it asymmetrically benefits state actors while imposing suppression on non-state actors. It is not a simple snare (pure extraction) because some coordination function exists (threshold clarity enables predictability for states and humanitarian agencies). The Tangled Rope classification for the state-centric reading (if that were the claimed_type, which it is not) would capture this hybrid accurately. The Snare classification we have chosen emphasizes the extraction-dominant perspective of irregular combatants and reflects that suppression is high (0.68) and effective extraction (χ) is substantial. The mandatrophy is resolved by the perspectival frame: from the beneficiary state perspective, this is rope-like coordination; from the victim perspective, this is snare-like extraction; the true structural picture is the asymmetry between these perspectives.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    threshold_objectivity_ambiguity,
    'Are the ''intensity and organization'' thresholds objective criteria for armed conflict, or contestable institutional definitions that preserve state discretion?',
    'Comparative analysis of threshold application across jurisdictions; examination of whether state practice converges on consistent metrics or diverges based on political interest; historical analysis of how thresholds have shifted in response to state security needs vs. humanitarian pressure',
    'If objective: the state-centric reading is valid—CA3 scope is genuinely limited by conflict characteristics. If contestable: the state-centric reading is a power interpretation—governments use threshold flexibility to exclude inconvenient conflicts from humanitarian protection. This determines whether the constraint is a rope (shared understanding of a objective gate) or a snare (states retain definitional power).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threshold_objectivity_ambiguity, conceptual, 'Whether thresholds are objective criteria or contestable institutional definitions').

omega_variable(
    suppression_mechanism_state_definition_power,
    'What is the mechanism by which the state-centric reading suppresses alternative interpretations of CA3 scope—through legal formalism, enforcement capacity, or something else?',
    'Analysis of how states block or marginalize expansive and customary-law readings in treaty negotiations, judicial proceedings, and enforcement mechanisms. Documentation of whether the suppression is active (states explicitly reject alternatives) or passive (state practice crowds out alternatives through institutional entrenchment).',
    'If suppression is active legal exclusion: the constraint is a snare with deliberate extraction. If suppression is institutional inertia: the constraint approaches tangled_rope territory (coordination + extraction). Drives classification at intermediate thresholds (extractiveness 0.50–0.65).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(suppression_mechanism_state_definition_power, empirical, 'Mechanism by which state-centric reading suppresses alternatives').

omega_variable(
    gray_zone_conflict_proliferation,
    'Is the proliferation of conflicts that fall below the state-centric threshold (counterinsurgency, counter-terrorism, internal security operations) a consequence of the threshold''s existence, or an independent structural feature of modern conflict?',
    'Historical comparative analysis: examine conflict classification patterns before and after CA3 came into force (1950s–present). Compare jurisdictions with expansive interpretations vs. state-centric interpretations to assess whether state-centric scope encourages gray-zone conflict adoption. Trace specific conflicts that governments reclassified as ''internal security'' or ''below threshold'' after initial conflict classification.',
    'If threshold existence encourages gray-zone strategy: the suppression value increases (0.68 → 0.75+) because the constraint actively incentivizes extractive conflict forms. If gray-zone proliferation is independent: suppression reflects state discretion rather than constraint-driven incentive distortion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gray_zone_conflict_proliferation, empirical, 'Whether threshold existence incentivizes gray-zone conflict adoption').

omega_variable(
    reading_kernel_contestation,
    'Is this constraint (the state-centric reading) a defensible interpretation of CA3''s actual text and history, or a power interpretation that privileges state interests over humanitarian scope?',
    'Textual analysis of CA3 language (''armed conflict'' definition, applicability triggers). Historical analysis of drafting intent (ICRC deliberations, state negotiations in 1949). Comparative examination of how the expansive and customary-law readings derive from the same text and history. Assessment of whether state practice has genuinely coalesced on the state-centric reading or whether divergence persists.',
    'If textually defensible: the reading is a live reading among others (coexists_with). If textually problematic: the reading forecloses alternatives through institutional power rather than interpretive logic, and should reclassify to higher extractiveness (0.58 → 0.65+). This omega documents the kernel contestation itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_kernel_contestation, conceptual, 'Whether state-centric reading is textually defensible or power interpretation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(common_article_3_scope__state_centric_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ca3_state_theater_t0, common_article_3_scope__state_centric_reading, theater_ratio, 0, 0.48).
narrative_ontology:measurement(ca3_state_theater_t15, common_article_3_scope__state_centric_reading, theater_ratio, 15, 0.5).
narrative_ontology:measurement(ca3_state_theater_t30, common_article_3_scope__state_centric_reading, theater_ratio, 30, 0.52).

% Extraction over time
narrative_ontology:measurement(ca3_state_extract_t0, common_article_3_scope__state_centric_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(ca3_state_extract_t15, common_article_3_scope__state_centric_reading, base_extractiveness, 15, 0.54).
narrative_ontology:measurement(ca3_state_extract_t30, common_article_3_scope__state_centric_reading, base_extractiveness, 30, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(ca3_state_supp_t0, common_article_3_scope__state_centric_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(ca3_state_supp_t15, common_article_3_scope__state_centric_reading, suppression_requirement, 15, 0.64).
narrative_ontology:measurement(ca3_state_supp_t30, common_article_3_scope__state_centric_reading, suppression_requirement, 30, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(common_article_3_scope__state_centric_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(common_article_3_scope__state_centric_reading, common_article_3_scope__expansive_human_rights_reading).
narrative_ontology:affects_constraint(common_article_3_scope__state_centric_reading, common_article_3_scope__icrc_customary_reading).
narrative_ontology:affects_constraint(common_article_3_scope__state_centric_reading, non_international_armed_conflict_definition).
narrative_ontology:affects_constraint(common_article_3_scope__state_centric_reading, irregular_combatant_legal_status).

% DUAL FORMULATION NOTE:
% This constraint is part of the COMMON_ARTICLE_3_SCOPE kernel family. Three constraint stories instantiate three distinct readings of CA3's scope and application. Each reading has its own ε value, beneficiary/victim structure, and classification. The state-centric reading (ε=0.58, Snare) emphasizes state discretion and threshold control. The expansive reading (ε=0.62, Tangled Rope) emphasizes humanitarian floors and inclusivity. The customary reading (ε=0.38, Tangled Rope) emphasizes evolving state practice. All three are linked via network.affects_constraints. The readings are not measurement variants of one constraint — they are genuinely different institutional arrangements with different beneficiary/victim structures and different effective extraction mechanisms. The ε values differ because the readings instantiate different distributional facts (who benefits, who bears costs) and different suppression mechanisms (what alternatives are foreclosed for each actor).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
