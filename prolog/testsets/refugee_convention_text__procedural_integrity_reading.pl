% ============================================================================
% CONSTRAINT STORY: refugee_convention_text__procedural_integrity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_refugee_convention_text__procedural_integrity_reading, []).

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
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: refugee_convention_text__procedural_integrity_reading
 *   human_readable: Refugee Convention Text — Procedural Integrity Reading
 *   domain: international_law/migration_governance/human_rights
 *
 * SUMMARY:
 *   The Refugee Convention's text commits to fair individualized assessment
 *   as a procedural safeguard for refugee status determination. This
 *   procedural integrity reading instantiates ONE interpretation of that
 *   commitment: protection thresholds may be flexible, state sovereignty is
 *   recognized, but the process of evaluation is non-negotiable. An applicant
 *   must receive individualized, impartial review based on the facts of their
 *   case. This reading sits between two sibling readings: the restrictive
 *   sovereignty reading (states may narrow definitions and gatekeep access to
 *   procedure entirely) and the expansive humanitarian reading (obligations
 *   extend to all persons in need regardless of procedural access). The
 *   procedural integrity reading is a middle position: it permits offshore
 *   processing, resource-constrained adjudication, and flexible outcome
 *   thresholds, but only if the procedural commitment is honored. The
 *   constraint exhibits tangled rope structure: genuine procedural
 *   coordination for in-territory claimants who access the system, coupled
 *   with extractive gatekeeping that determines who reaches procedure in the
 *   first place. The rising extractiveness over the measurement interval
 *   reflects increasing sophistication of offshore gatekeeping mechanisms
 *   (Australia's offshore processing, EU externalization agreements, UK
 *   offshore asylum processing proposals) that preserve procedural form while
 *   structurally restricting access.
 *
 * KEY AGENTS:
 *   - Adjudicating States (Proceduralist): States genuinely committed to fair procedure; experience constraint as coordination. Beneficiaries of clear legal rules and legitimacy.
 *   - Adjudicating States (Deterrent): States prioritizing migration control; use offshore processing to extract deterrence while maintaining procedural facade. Primary beneficiaries of the constraint's extractive dimension.
 *   - In-Territory Asylum Seekers: Can access procedure but face gatekeeping barriers (cost, delay, hostile apparatus). Receive procedural protection but within constrained access framework.
 *   - Offshore Asylum Seekers: Structurally excluded from in-territory procedure; trapped outside the constraint's scope. Whether they are victims depends on reading choice.
 *   - Human Rights Advocacy Coalition: UNHCR, legal NGOs, regional commissions. Organized actors with partial agency — can litigate procedure, cannot mandate access.
 *   - International Monitoring Bodies: Courts, treaty bodies, fact-finding missions. Enforce the procedural reading against states; provide review mechanism but limited enforcement power.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(refugee_convention_text__procedural_integrity_reading, 0.52).
domain_priors:suppression_score(refugee_convention_text__procedural_integrity_reading, 0.68).
domain_priors:theater_ratio(refugee_convention_text__procedural_integrity_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(refugee_convention_text__procedural_integrity_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(refugee_convention_text__procedural_integrity_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(refugee_convention_text__procedural_integrity_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(refugee_convention_text__procedural_integrity_reading, tangled_rope).
narrative_ontology:human_readable(refugee_convention_text__procedural_integrity_reading, "Refugee Convention Text — Procedural Integrity Reading").
narrative_ontology:topic_domain(refugee_convention_text__procedural_integrity_reading, "international_law/migration_governance/human_rights").

domain_priors:requires_active_enforcement(refugee_convention_text__procedural_integrity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(refugee_convention_text__procedural_integrity_reading, 'e41f7475-9bae-4ac0-9c4e-36665d808d1d').
narrative_ontology:cs_kernel_codification('e41f7475-9bae-4ac0-9c4e-36665d808d1d', fixed_text).
narrative_ontology:cs_authority_grounding('e41f7475-9bae-4ac0-9c4e-36665d808d1d', lineage).
narrative_ontology:cs_interpretation_layer_present('e41f7475-9bae-4ac0-9c4e-36665d808d1d').
narrative_ontology:cs_reading_relation('e41f7475-9bae-4ac0-9c4e-36665d808d1d', refugee_convention_text__restrictive_sovereignty_reading, coexists_with).
narrative_ontology:cs_reading_relation('e41f7475-9bae-4ac0-9c4e-36665d808d1d', refugee_convention_text__expansive_humanitarian_reading, influences).
narrative_ontology:cs_axiom('e41f7475-9bae-4ac0-9c4e-36665d808d1d', foundational, procedural_integrity_non_negotiable).
narrative_ontology:cs_axiom_status(procedural_integrity_non_negotiable, holdable).
narrative_ontology:cs_axiom_grounding('e41f7475-9bae-4ac0-9c4e-36665d808d1d', procedural_integrity_non_negotiable, deontological).
narrative_ontology:cs_axiom('e41f7475-9bae-4ac0-9c4e-36665d808d1d', foundational, outcome_thresholds_state_flexible).
narrative_ontology:cs_axiom_status(outcome_thresholds_state_flexible, holdable).
narrative_ontology:cs_axiom_grounding('e41f7475-9bae-4ac0-9c4e-36665d808d1d', outcome_thresholds_state_flexible, deontological).
narrative_ontology:cs_created_at('e41f7475-9bae-4ac0-9c4e-36665d808d1d', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(refugee_convention_text__procedural_integrity_reading, refugee_convention_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(refugee_convention_text__procedural_integrity_reading, adjudicating_states).
narrative_ontology:constraint_beneficiary(refugee_convention_text__procedural_integrity_reading, procedural_gatekeepers).
narrative_ontology:constraint_victim(refugee_convention_text__procedural_integrity_reading, asylum_seekers_offshore).
narrative_ontology:constraint_victim(refugee_convention_text__procedural_integrity_reading, procedurally_excluded_claimants).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: OFFSHORE ASYLUM SEEKER (SNARE) — Located outside territorial jurisdiction, cannot access the procedural safeguards that define protection. Trapped by geography; procedural guarantees are only meaningful if one can reach the forum where they are enforced. Maximum extraction: the reading's core commitment (fair individualized assessment) is structurally inaccessible.
constraint_indexing:constraint_classification(refugee_convention_text__procedural_integrity_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: IN-TERRITORY ASYLUM SEEKER (TANGLED ROPE) — Can access procedure but faces high barriers (legal representation costs, administrative delays, hostile state apparatus). Benefits from the procedural commitment itself (right to assessment exists); bears extraction through delay, resource scarcity, and procedural manipulation. Mixed structure: genuine procedural protection for those who reach the adjudication forum, but constrained by gatekeeping.
constraint_indexing:constraint_classification(refugee_convention_text__procedural_integrity_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: ADJUDICATING STATE — PROCEDURALIST READING (ROPE) — For a state genuinely committed to procedural integrity, the constraint is coordination: the Convention establishes a shared epistemic and procedural standard that reduces transaction costs and legitimacy contestation. The state benefits from clear rules (predicability) and delegates gatekeeping burden to formalized process. Low experienced extraction because the state genuinely wants reliable procedure.
constraint_indexing:constraint_classification(refugee_convention_text__procedural_integrity_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: ADJUDICATING STATE — DETERRENCE POSTURE (TANGLED ROPE) — When a state prioritizes migration control, the procedural reading permits offshore processing that technically preserves procedure while reducing access. The state extracts migration selectivity (deterrent effect on marginal claimants) while maintaining procedural facade. Coordination function (procedure legitimizes decisions); extraction function (deterrence narrows pool reaching procedure). Constrained by international monitoring and domestic litigation.
constraint_indexing:constraint_classification(refugee_convention_text__procedural_integrity_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: HUMAN RIGHTS ADVOCACY COALITION (TANGLED ROPE) — Organized actors (UNHCR, legal NGOs, regional human rights commissions) see the procedural reading as foundational but exploitable. The reading's commitment to 'fair individualized assessment' is a genuine coordination mechanism — it provides legal standing for advocacy. But the reading's silence on access (offshore processing permitted if procedure is fair) permits extraction through gatekeeping. The coalition has partial agency — can litigate procedure, cannot mandate access.
constraint_indexing:constraint_classification(refugee_convention_text__procedural_integrity_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER — NATURAL LAW VIEW (MOUNTAIN) — From a universalized analytical perspective, the procedural integrity requirement appears as an immutable logical constraint: any legitimate legal system requires fair individualized assessment of status claims. Procedure is non-negotiable; this appears as a structural necessity of law itself. However, the distributional data (who accesses procedure, who is offshore, who benefits from gatekeeping) suggests this is a naturalized institutional arrangement, not a law of nature. False-summit candidate.
constraint_indexing:constraint_classification(refugee_convention_text__procedural_integrity_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(refugee_convention_text__procedural_integrity_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(refugee_convention_text__procedural_integrity_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(refugee_convention_text__procedural_integrity_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(refugee_convention_text__procedural_integrity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(refugee_convention_text__procedural_integrity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): The procedural reading permits significant gatekeeping through offshore processing and location-based access barriers, yet maintains the fiction that procedure is available. Extractiveness is moderate-high because the reading simultaneously commits to fair assessment (coordination logic) and permits structural exclusion from procedure (extraction logic). The reading does not foreclose offshore processing, which is the primary extraction mechanism. Suppression (0.68): High. Multiple mechanisms suppress access: geographic remoteness, documentation barriers, legal representation scarcity, administrative delay, and state hostility. The procedural reading does not mandate removal of these barriers — it only requires that procedure, once accessed, be fair. Suppression is thus a feature of the constraint, not a violation of it. Theater ratio (0.58): Moderate-high. Procedure itself is largely genuine (adjudicators do assess cases), but the procedural framework obscures the prior gatekeeping that determines who accesses procedure. The procedure is real; the claim that procedure is accessible is theatrical. Rising theater over the interval reflects increasing sophistication in maintaining procedural legitimacy while restricting access (documentation requirements, offshore processing with video hearings, third-country processing agreements). The rising suppression_requirement metric reflects increasing institutional investment in gatekeeping (border enforcement, offshore detention, cooperation with third countries) required to maintain the deterrent effect.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits extreme perspectival divergence. The proceduralist state sees Rope (procedure coordinates legitimate review; state benefits from clear rules). The deterrent state sees Tangled Rope (procedure legitimizes migration control while constraining states through appeal mechanisms and international review). In-territory claimants see Tangled Rope (procedure exists but access is constrained). Offshore claimants see Snare (excluded from procedure entirely; no exit). The human rights coalition sees Tangled Rope (procedure is genuinely protective but exploitable through gatekeeping). The analytical observer risks seeing Mountain (procedure is non-negotiable logic of law) but the structural data reveals this as a false summit: procedure's inevitability conceals the reading's choice to permit offshore gatekeeping. This perspectival range is diagnostic: it reveals that the procedural integrity reading is a reading — a contingent choice about which elements of refugee protection are negotiable (thresholds, outcomes) and which are non-negotiable (procedure). Alternative readings would shift the negotiable/non-negotiable boundary.
 *
 * DIRECTIONALITY LOGIC:
 *   The procedural integrity reading creates asymmetric directionality flows. Beneficiary states (institutional/arbitrage) derive low d from their structural position as rule-makers and procedure administrators — they can exit, can modify rules, can selectively apply procedure. Victim claimants (powerless/trapped) derive high d from being subject to gatekeeping beyond their control. In-territory claimants (moderate/constrained) occupy an intermediate position: they can technically access procedure but face substantial barriers. The offshore claimant perspective shows maximum d (1.0 equivalent) because offshore location strips access entirely — the claimant is subject to the constraint (exclusion from procedure) with zero exit capacity. The human rights coalition (organized/constrained) occupies a mixed position: they can litigate procedure but cannot mandate geographic access. The proceduralist state (institutional/arbitrage) experiences low d when genuinely committed to procedure, because the procedural commitment enables them to legitimize decisions and reduce contestation. The deterrent state (institutional/constrained) experiences higher d because it must invest resources in gatekeeping to maintain the deterrent effect while preserving procedural appearance.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy resolves through explicit recognition of reading choice. The procedural integrity reading is NOT claiming that the Refugee Convention has one objectively correct type. It is claiming that IF one adopts the procedural reading (procedure is non-negotiable, outcomes/access are flexible), THEN the constraint is Tangled Rope with significant extractive gatekeeping. A state adopting the restrictive sovereignty reading would classify the constraint as Rope or even Scaffold (temporary protection structures). A state adopting the expansive humanitarian reading would classify it as Tangled Rope or Snare, but with a different victim set (offshore claimants included in victims). The mandatrophy is resolved by observing that all three readings are internally coherent; the reading choice determines the classification. The engine's false-summit detector flags the analytical mountain perspective as a naturalization of the procedural reading — procedure appears inevitable only if one accepts the procedural commitment as non-negotiable, which is itself a reading choice.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    offshore_processing_procedure_sufficiency,
    'Can fair individualized assessment occur offshore without territorial presence, or does procedural integrity require access to in-territory review mechanisms and appeal processes?',
    'Comparative analysis of outcomes for offshore vs in-territory procedural assessments; review of appeal success rates, evidence quality, and representation capacity in each context; documentation of institutional capacity for offshore adjudication',
    'If offshore procedure is genuinely sufficient: procedural reading permits deterrent gatekeeping. If offshore procedure is structurally compromised: offshore processing violates the reading''s core axiom, reclassifying as snare for offshore claimants; in-territory asymmetry becomes indefensible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(offshore_processing_procedure_sufficiency, empirical, 'Whether offshore processing can satisfy procedural integrity requirement').

omega_variable(
    reading_choice_determines_victim_set,
    'This reading instantiates the procedural integrity commitment, which reading governs for claimants excluded from procedure (offshore) vs those accessing procedure (in-territory)?',
    'Institutional analysis: which reading (procedural_integrity_reading, restrictive_sovereignty_reading, or expansive_humanitarian_reading) do states invoke for offshore gatekeeping? Which reading governs appeals and review mechanisms? What happens when the readings conflict (e.g., sovereignty permits offshore processing, but procedural integrity requires access)?',
    'The victim set is reading-dependent. Under procedural_integrity_reading, offshore claimants are excluded from the scope of protection (outside procedure = outside the constraint''s victim definition). Under expansive_humanitarian_reading, offshore claimants would be in-scope (humanitarian obligations apply regardless of location). The conflict is not empirical — it is a choice of which reading governs.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_choice_determines_victim_set, conceptual, 'How reading choice determines victim set for offshore claimants').

omega_variable(
    gatekeeping_as_extraction_or_necessity,
    'Is procedural gatekeeping (location-based, resource-based, or documentation-based access barriers) a legitimate protection mechanism or an extraction mechanism disguised as procedure?',
    'Institutional comparison: analyze states that maintain robust in-territory procedure vs those that route claimants offshore. Track outcomes (approval rates, appeal success, repatriation/harm data) in each institutional context. Identify whether gatekeeping differences predict protection outcomes or merely redistribute claimants.',
    'If gatekeeping is extractive: the reading''s claim that ''procedure integrity is non-negotiable'' conceals selective access (procedural facade). If gatekeeping is legitimate protection mechanism: the reading accommodates state sovereignty while maintaining procedural safeguards for those in scope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gatekeeping_as_extraction_or_necessity, empirical, 'Whether gatekeeping functions as protection or extraction').

omega_variable(
    procedure_as_coordination_or_deterrent_infrastructure,
    'Does the procedural commitment fundamentally coordinate legitimate review (all states need reliable procedure), or does it provide infrastructure for deterrence (procedure legitimizes reduced access)?',
    'Comparative analysis of state behavior: examine whether states invest in procedure to ensure reliable review (coordination logic) or maintain minimum procedure while maximizing gatekeeping costs (deterrence logic). Track resource allocation to adjudication capacity vs border enforcement.',
    'If primarily coordination: the reading is genuinely Rope from the proceduralist state''s perspective; extraction is secondary and contestable. If primarily deterrence infrastructure: procedure is a facade; the constraint is primarily Snare from the asylum seeker''s perspective, with procedure as theater.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(procedure_as_coordination_or_deterrent_infrastructure, empirical, 'Whether procedural commitment functions as coordination or deterrence infrastructure').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(refugee_convention_text__procedural_integrity_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(refconv_proc_tr_t0, refugee_convention_text__procedural_integrity_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement(refconv_proc_tr_t10, refugee_convention_text__procedural_integrity_reading, theater_ratio, 10, 0.5).
narrative_ontology:measurement(refconv_proc_tr_t20, refugee_convention_text__procedural_integrity_reading, theater_ratio, 20, 0.58).

% Extraction over time
narrative_ontology:measurement(refconv_proc_be_t0, refugee_convention_text__procedural_integrity_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(refconv_proc_be_t10, refugee_convention_text__procedural_integrity_reading, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(refconv_proc_be_t20, refugee_convention_text__procedural_integrity_reading, base_extractiveness, 20, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(refconv_proc_su_t0, refugee_convention_text__procedural_integrity_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(refconv_proc_su_t10, refugee_convention_text__procedural_integrity_reading, suppression_requirement, 10, 0.62).
narrative_ontology:measurement(refconv_proc_su_t20, refugee_convention_text__procedural_integrity_reading, suppression_requirement, 20, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(refugee_convention_text__procedural_integrity_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(refugee_convention_text__procedural_integrity_reading, refugee_convention_text__restrictive_sovereignty_reading).
narrative_ontology:affects_constraint(refugee_convention_text__procedural_integrity_reading, refugee_convention_text__expansive_humanitarian_reading).
narrative_ontology:affects_constraint(refugee_convention_text__procedural_integrity_reading, offshore_processing_legitimacy).
narrative_ontology:affects_constraint(refugee_convention_text__procedural_integrity_reading, asylum_seeker_legal_standing).

% DUAL FORMULATION NOTE:
% The refugee_convention_text kernel has three constraint stories corresponding to three distinct readings. This is the PROCEDURAL_INTEGRITY_READING. The other readings (restrictive_sovereignty_reading, expansive_humanitarian_reading) are separate constraint files with different ε values, different victim sets, and different classifications. They are not different observables of one constraint — they are different structural claims about what the Convention requires. Each reading's ε captures the empirical status of that reading's interpretation within international law. This decomposition follows the ε-invariance principle: if reading choice changes ε, the readings instantiate different constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(refugee_convention_text__procedural_integrity_reading, institutional, 0.35).
constraint_indexing:directionality_override(refugee_convention_text__procedural_integrity_reading, institutional, 0.62).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
