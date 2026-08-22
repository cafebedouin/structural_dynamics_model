% ============================================================================
% CONSTRAINT STORY: honor_settlement_legitimacy__drop_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_settlement_legitimacy__drop_reading, []).

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
 *   constraint_id: honor_settlement_legitimacy__drop_reading
 *   human_readable: Honor-Based Dispute Settlement in Dueling Residual Communities
 *   domain: social/legal/cultural
 *
 * SUMMARY:
 *   This constraint describes honor-based dispute settlement (specifically
 *   dueling) as it persisted in geographic and social niches throughout the
 *   18th and 19th centuries despite legal suppression and cultural
 *   delegitimization in the broader society. The drop_reading instantiates
 *   the perspective that honor culture remained a live—though increasingly
 *   marginalized—option for settling disputes in specific regional
 *   communities, particularly among military and aristocratic populations.
 *   Dueling was not cognitively eliminated from the normative repertoire;
 *   rather, it contracted geographically and socially while retaining
 *   legitimacy and functional meaning for those whose identity remained fused
 *   to honor codes. This reading stands in tension with the
 *   contraction_reading (which frames dueling's decline as a complete
 *   cognitive-framework transformation) and the composite_reading (which
 *   treats decline as overdetermined). The drop_reading emphasizes
 *   persistence despite suppression: the practice remained organizationally
 *   viable, participants maintained it as a meaningful institution, and
 *   alternatives never fully closed off—making it a tangled rope
 *   (coordination of honor settlement + extraction from state enforcement and
 *   non-dueling society) rather than a mountain or a snare.
 *
 * KEY AGENTS:
 *   - honor_culture_adherents: agenda-setters (identity-locked, moderate power) — organize the dueling system, maintain challenge protocols, execute and adjudicate contests
 *   - state_monopoly_enforcement_agents: payers (institutional power) — bear costs of prosecuting dueling, managing legitimacy gap, allocating enforcement resources
 *   - non_dueling_society_members: payers (organized, mobile) — bear costs of enforcement uncertainty and cultural friction; subsidize suppression efforts
 *   - insulted_non_duelers: excluded, powerless — cannot access the settlement mechanism despite being affected by its legitimacy claims
 *   - legal_reform_advocates: observer seats — push for full criminalization and cultural delegitimization
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_settlement_legitimacy__drop_reading, 0.68).
domain_priors:suppression_score(honor_settlement_legitimacy__drop_reading, 0.72).
domain_priors:theater_ratio(honor_settlement_legitimacy__drop_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_settlement_legitimacy__drop_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(honor_settlement_legitimacy__drop_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(honor_settlement_legitimacy__drop_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_settlement_legitimacy__drop_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(honor_settlement_legitimacy__drop_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_settlement_legitimacy__drop_reading, tangled_rope).
narrative_ontology:human_readable(honor_settlement_legitimacy__drop_reading, "Honor-Based Dispute Settlement in Dueling Residual Communities").
narrative_ontology:topic_domain(honor_settlement_legitimacy__drop_reading, "social/legal/cultural").

domain_priors:requires_active_enforcement(honor_settlement_legitimacy__drop_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_settlement_legitimacy__drop_reading, 'a81d776b-ba22-4785-9c30-43ba27a3be1a').
narrative_ontology:cs_kernel_codification('a81d776b-ba22-4785-9c30-43ba27a3be1a', distributed).
narrative_ontology:cs_authority_grounding('a81d776b-ba22-4785-9c30-43ba27a3be1a', lineage).
narrative_ontology:cs_interpretation_layer_present('a81d776b-ba22-4785-9c30-43ba27a3be1a').
narrative_ontology:cs_reading_relation('a81d776b-ba22-4785-9c30-43ba27a3be1a', honor_settlement_legitimacy__contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('a81d776b-ba22-4785-9c30-43ba27a3be1a', honor_settlement_legitimacy__composite_reading, influences).
narrative_ontology:cs_axiom('a81d776b-ba22-4785-9c30-43ba27a3be1a', foundational, honor_settlement_cognitively_persistent).
narrative_ontology:cs_axiom_status(honor_settlement_cognitively_persistent, holdable).
narrative_ontology:cs_axiom_grounding('a81d776b-ba22-4785-9c30-43ba27a3be1a', honor_settlement_cognitively_persistent, conventional).
narrative_ontology:cs_axiom('a81d776b-ba22-4785-9c30-43ba27a3be1a', secondary, identity_lock_prevents_normative_exit).
narrative_ontology:cs_axiom_status(identity_lock_prevents_normative_exit, holdable).
narrative_ontology:cs_axiom_grounding('a81d776b-ba22-4785-9c30-43ba27a3be1a', identity_lock_prevents_normative_exit, empirically_contingent).
narrative_ontology:cs_reference_frame('a81d776b-ba22-4785-9c30-43ba27a3be1a', honor_as_legitimate_remedy).
narrative_ontology:cs_drift_state('a81d776b-ba22-4785-9c30-43ba27a3be1a', post_legal_reform_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('a81d776b-ba22-4785-9c30-43ba27a3be1a', '').
narrative_ontology:cs_kernel_id(honor_settlement_legitimacy__drop_reading, honor_settlement_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_settlement_legitimacy__drop_reading, honor_culture_adherents).
narrative_ontology:constraint_victim(honor_settlement_legitimacy__drop_reading, state_monopoly_enforcement_agents).
narrative_ontology:constraint_victim(honor_settlement_legitimacy__drop_reading, non_dueling_society_members).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% A community of residual practitioners who maintain dueling as a legitimate mechanism for settling disputes, restoring reputation after insult, and adjudicating claims of honor. They frame dueling as a rational response to situations where state legal institutions fail to provide timely or satisfactory redress, and where reputation damage requires immediate personal vindication. Their identity is constituted through adherence to honor codes; exit would require renouncing core self-conception. They administer the practice through coded challenges, seconds, and ritualized enforcement of outcomes among themselves.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__drop_reading, honor_culture_adherents, agenda_setter,
    moderate, generational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(honor_settlement_legitimacy__drop_reading, honor_culture_adherents, beneficiary).

% Law enforcement and courts charged with maintaining the state's monopoly on legitimate violence and dispute settlement. They bear the cost of prosecuting dueling cases, accommodating residual honor settlements outside formal legal channels, and managing the legitimacy gap created by the fringe practice—their authority is partially undermined by the continued operation of parallel adjudication systems. They must allocate enforcement resources to suppress or tolerate dueling depending on political pressure and geographic location.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__drop_reading, state_monopoly_enforcement_agents, payer,
    institutional, generational, analytical, regional).

% Populations in the same geographic regions who have accepted state monopoly on legitimate dispute settlement and renounced honor-based violence. They bear costs through enforcement uncertainty (duels occur despite suppression laws), through the cultural friction created by the parallel system's persistence, and through the demographic losses from fatal duels. They also subsidize enforcement spending on suppression efforts that benefit from majoritarian rule preference.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__drop_reading, non_dueling_society_members, payer,
    organized, biographical, mobile, regional).

% Individuals outside honor culture who experience insults but cannot respond through dueling—either because they reject the practice or lack the social standing to issue or accept a challenge. They are excluded from the dispute settlement mechanism entirely; their grievances must route through state institutions, which may be slow or inadequate. They cannot participate in the honor-settlement conversation despite being affected by its legitimacy claims.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__drop_reading, insulted_non_duelers, excluded,
    powerless, biographical, constrained, regional).

% Reformers and jurists pushing for full criminalization of dueling and cultural delegitimization of honor-based settlement. They argue that persistence of the fringe practice undermines rule of law and that the arrangement extracts from non-duelers through enforcement costs and legitimacy erosion. They seek to shift the normative repertoire by establishing dueling as categorically criminal rather than regionally tolerated.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__drop_reading, legal_reform_advocates, observer,
    organized, generational, analytical, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(honor_settlement_legitimacy__drop_reading, honor_culture_adherents).
narrative_ontology:fixing_cost_class(honor_settlement_legitimacy__drop_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides rapid, binding dispute settlement for honor-related grievances (insults, reputation damage, unresolved claims) outside state legal channels; establishes clear protocols for challenge, combat, and outcome acceptance; creates mechanisms for restoring reputation when state institutions are perceived as too slow, biased, or inadequate to repair honor damage.
% TRANSFER_FUNCTION: Moves legitimacy from state monopoly on dispute settlement to decentralized honor-based arbitration; transfers risk of death or injury to participants (especially lower-ranking challengers); redistributes enforcement burden (state must suppress, duelers must manage their own contests and outcomes); creates cultural dominance of honor-code aesthetics in spaces where the practice persists.
% ABSENT_VOICES: Non-dueling populations in honor-culture regions whose grievances cannot be addressed through honor settlement due to lower status, and non-duelers who experience costs from enforcement uncertainty and legitimacy confusion. Reform advocates are present in some venues but excluded from the internal honor-culture conversation about whether dueling legitimately settles disputes.
% DISAPPEARANCE_RATIONALE: If dueling's residual legitimacy as a settlement mechanism disappeared overnight, honor-culture communities would experience immediate status degradation and would need alternative mechanisms for reputation repair; regions would shift more fully to state monopoly; but the constraint is already marginal—its disappearance would not reorganize society broadly, only reshape the options available within honor-culture enclaves and accelerate their integration into state-monopoly institutions.
% FOUNDING_PROBLEM: State legal institutions in early-modern and modern periods proved slow, uncertain, or biased in adjudicating disputes over honor, reputation, and insult. Individuals facing reputational damage without recourse to adequate legal remedy had no mechanism to restore status and establish public vindication except through personal contest. Honor culture provided a settlement mechanism that could execute rapidly and produce binding social recognition of outcome.
% FOUNDING_PROBLEM_CORROBORATION: Modern state legal systems have substantially improved in speed, procedural certainty, and availability, especially regarding libel, slander, and reputation harm. Legal reformers and contemporary jurists from outside honor-culture communities testify that the founding problem is resolved; honor-culture adherents contest this, arguing that state institutions remain inadequate for reputation restoration and that honor settlement solves a problem state law cannot—the problem is framed as persistent by insiders but solved by the broader institutional environment surrounding them.
narrative_ontology:disappearance_verdict(honor_settlement_legitimacy__drop_reading, world_rearranges).
narrative_ontology:founding_problem_status(honor_settlement_legitimacy__drop_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_settlement_legitimacy__drop_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(honor_settlement_legitimacy__drop_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_settlement_legitimacy__drop_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_settlement_legitimacy__drop_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(honor_settlement_legitimacy__drop_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(honor_settlement_legitimacy__drop_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68 at interval end) reflects the constraint's operation as a parallel justice system that extracts from the state (enforcement costs) and from non-duelers (subsidy of enforcement, cultural friction). The extractiveness rises over the interval as suppression machinery intensifies—the constraint becomes more purely extractive as its coordination function (settling honor disputes) becomes increasingly unnecessary (state legal reform progresses) and its persistence becomes increasingly dependent on active enforcement. Suppression (0.72) is substantial because the constraint's survival requires continuous suppression of the rival mechanism (state monopoly) and the rival norm (renunciation of honor-based violence). Theater_ratio (0.58) rises sharply over the interval (0.25 to 0.58) as the practice becomes increasingly performative: participants maintain dueling partly to restore reputation and partly to stage their allegiance to honor culture in the face of broader delegitimization. The measurement grid is shared across all three metrics and all time points, capturing the trajectory: founding problem (inadequate state justice) becomes increasingly solved over the interval; suppression machinery intensifies; theater ratio rises (function erodes, performance persists); extractiveness plateaus (the constraint persists for identity reasons, not because it extracts continuously growing rents). Accessibility_collapse (0.62) reflects the moderately closed alternatives: duelers cannot simply exit honor culture without renouncing identity; non-duelers cannot access honor settlement even if they wanted to; state cannot enforce perfectly while honor culture retains regional strength.
 *
 * PERSPECTIVAL GAP:
 *   The honor_culture_adherents seat experiences the constraint as legitimate coordination (solving the founding problem of inadequate honor remedy, which they contest is solved). The state_monopoly_enforcement_agents seat experiences it as extraction requiring costly suppression. The non_dueling_society_members seat experiences it as imposition (enforcement costs, cultural friction, demographic losses). The excluded insulted_non_duelers seat experiences it as denial of remedy. These divergences should compute differently in the engine because the structural positions differ: beneficiaries (adherents) with identity-locked exit get low directionality (d near beneficiary); payers (state, non-duelers) with constrained/mobile exit get higher directionality (targets); excluded parties get the strongest grievance signal. The constraint's type will likely compute as tangled_rope from each seat's structural position, though the beneficiary seat may perceive rope (pure coordination) while payer seats perceive snare (pure extraction), because of the identity-lock that prevents adherents from recognizing the extraction they depend on.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality for honor_culture_adherents should derive as low (~0.15-0.25): they are listed as beneficiaries and agenda-setters, their exit is identity_locked (very constrained), they actively maintain the system. Directionality for state_monopoly_enforcement_agents should derive as high (~0.70-0.85): they are victims bearing enforcement costs, their exit is analytical (forced participation by institutional mandate), their interests oppose the adherents'. Directionality for non_dueling_society_members should derive as moderate-to-high (~0.55-0.70): they are payers bearing enforcement subsidy and cultural friction, their exit is mobile (they can relocate or support cultural reform), but they lack power individually. These structural differences should produce divergent per-seat classifications despite a single shared constraint—the engine's job is to compute that divergence from the power + exit + beneficiary/victim declarations. No directionality overrides are needed if the base declarations are accurate.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint exhibits foundational mandatrophy: it was constructed to solve the founding problem of inadequate state remedy for honor damage, but by the interval's end (1900) the founding problem is substantially dead (state legal systems have improved, procedures exist for reputation harm). Yet the constraint persists, no longer by design (meeting the founding need) but by inertia and identity fusion. This mandatrophy separates the drop_reading from the contraction_reading: the drop_reading treats persistence despite mandatrophy as the salient fact—the constraint survives because participants' identities are fused to it, making it a tangled rope with high theater ratio and extractive character masked by identity-frame preservation. The composite_reading would treat this mandatrophy as one of multiple reinforcing suppression mechanisms; the contraction_reading would treat the cognitive obsolescence (the founding problem being dead) as enabling complete framework transformation. The drop_reading predicts that identity-locked participants will maintain the system even after it becomes economically irrational, that enforcement will intensify (the theater rises), and that the practice will contract regionally and socially but not disappear entirely—all of which the measurement series shows.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    identity_internalization_vs_rational_choice,
    'Do honor-culture adherents maintain dueling primarily through identity-fusion (self-concept constituted through honor codes) or through rational cost-benefit calculation (dueling remains instrumentally superior to state justice)?',
    'Post-suppression behavioral observation: if suppression levels rise and honor adherents continue dueling despite escalating legal penalties, identity-fusion is indicated. If they shift to alternatives when suppression becomes prohibitively costly, rational choice dominates. Interview data from historical records and oral histories distinguishing motivation types would provide direct evidence.',
    'High identity-fusion supports the drop_reading''s framing of inertial persistence and high theater ratio; high rational-choice would suggest the founding problem remains live and extraction is incidental to coordination. Identity-lock also implies suppression has internalized components (the adherent carries the honor-code restriction with them even after exit from dueling subculture).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_internalization_vs_rational_choice, empirical, 'Whether dueling persistence is driven by identity-fusion or rational instrumental calculation.').

omega_variable(
    state_legal_reform_efficacy,
    'To what extent did state legal reforms actually provide adequate remedy for honor damage (libel law, defamation remedies, procedural acceleration), and to what extent do honor adherents'' founding-problem claims remain empirically grounded?',
    'Comparative legal analysis of state remedies available at 1850 vs. 1900; case law records showing whether libel/slander suits achieved reputation restoration outcomes comparable to dueling; testimony from honor adherents about whether state remedies would have satisfied their founding-problem criterion.',
    'If state remedies were genuinely adequate, the founding problem is dead and the constraint is pure extraction (snare). If state remedies remained inadequate for honor damage, the founding problem persists and the constraint retains genuine coordination function (rope/tangled rope). This uncertainty drives the drop_reading''s claim that the problem status is contested.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_legal_reform_efficacy, empirical, 'Whether the state legal reform actually solved the founding problem or left a residual gap that dueling addressed.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the suppression of dueling primarily structural (state enforcement, legal penalties, institutional barriers) or internalized (cultural shame, cognitive delegitimization, self-enforced renunciation)?',
    'If ex-duelers who exit the community continue to renounce honor-based violence despite absence of state enforcement, suppression is partially internalized. If suppression completely dissolves when state enforcement ends, it is purely structural. Historical natural experiments (regions/periods with weak enforcement) provide evidence.',
    'High internalization supports the contraction_reading (cultural framework shift) over the drop_reading (persistent option). High structural suppression supports the drop_reading (the practice persists because enforcement is imperfect, not because adherents have transformed their framework). This also affects the effective suppression metric: internalized suppression may report as lower structural metric but carry stronger behavioral effect.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'The balance between structural enforcement and internalized cultural delegitimization in suppressing dueling.').

omega_variable(
    reading_foreclosure_via_axiom_override,
    'Does the cognitive delegitimization of honor culture in the contraction_reading logically foreclose the drop_reading''s claim that honor remains a live normative option?',
    'Axiomatic analysis: if the contraction_reading''s foundational axiom (honor-based dispute settlement becomes categorically unthinkable) is true, can the drop_reading''s axiom (honor remains cognitively live for residual adherents) coexist, or do they contradict? If they can coexist (different populations maintain different frameworks), the readings coexist; if they contradict (one framework sweeps all), one forecloses the other.',
    'Foreclosure would mean one reading is empirically false; coexistence means the empirical question is whether populations actually partition into different frameworks or whether there is a unified cognitive transformation. This determines reading_relations in the cs_structure block.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_foreclosure_via_axiom_override, conceptual, 'Whether the drop_reading and contraction_reading logically foreclose each other or coexist as live positions.').

omega_variable(
    geographic_and_social_scope_variation,
    'Did dueling persist with equal strength across all honor-culture regions and classes, or was persistence concentrated in specific geographic/military/aristocratic contexts with different suppression intensities?',
    'Quantitative analysis of dueling frequency by region, class, military vs. civilian context, and over time. If persistence is concentrated (e.g., military duels continue while civilian duels disappear), the constraint is better modeled as multiple constraints per region rather than one global constraint, or as identity_coordination type constrained to specific spatial and organizational niches.',
    'High geographic/class variance supports decomposing the constraint into regional or status-specific stories; uniform decline supports treating it as one constraint. This affects spatial_scope declaration and the interpretation of accessibility_collapse (alternatives collapsed completely for military/aristocratic males but not for bourgeois populations).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(geographic_and_social_scope_variation, empirical, 'Whether dueling persistence was uniform across honor-culture space or concentrated in specific regions and classes.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_settlement_legitimacy__drop_reading, 1700, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hono_tr_t1700, honor_settlement_legitimacy__drop_reading, theater_ratio, 1700, 0.25).
narrative_ontology:measurement_basis(hono_tr_t1700, projected).
narrative_ontology:measurement(hono_tr_t1750, honor_settlement_legitimacy__drop_reading, theater_ratio, 1750, 0.3).
narrative_ontology:measurement_basis(hono_tr_t1750, observed).
narrative_ontology:measurement(hono_tr_t1800, honor_settlement_legitimacy__drop_reading, theater_ratio, 1800, 0.38).
narrative_ontology:measurement_basis(hono_tr_t1800, observed).
narrative_ontology:measurement(hono_tr_t1850, honor_settlement_legitimacy__drop_reading, theater_ratio, 1850, 0.48).
narrative_ontology:measurement_basis(hono_tr_t1850, observed).
narrative_ontology:measurement(hono_tr_t1875, honor_settlement_legitimacy__drop_reading, theater_ratio, 1875, 0.55).
narrative_ontology:measurement_basis(hono_tr_t1875, observed).
narrative_ontology:measurement(hono_tr_t1900, honor_settlement_legitimacy__drop_reading, theater_ratio, 1900, 0.58).
narrative_ontology:measurement_basis(hono_tr_t1900, observed).

% Extraction over time
narrative_ontology:measurement(hono_be_t1700, honor_settlement_legitimacy__drop_reading, base_extractiveness, 1700, 0.52).
narrative_ontology:measurement_basis(hono_be_t1700, projected).
narrative_ontology:measurement(hono_be_t1750, honor_settlement_legitimacy__drop_reading, base_extractiveness, 1750, 0.58).
narrative_ontology:measurement_basis(hono_be_t1750, observed).
narrative_ontology:measurement(hono_be_t1800, honor_settlement_legitimacy__drop_reading, base_extractiveness, 1800, 0.63).
narrative_ontology:measurement_basis(hono_be_t1800, observed).
narrative_ontology:measurement(hono_be_t1850, honor_settlement_legitimacy__drop_reading, base_extractiveness, 1850, 0.66).
narrative_ontology:measurement_basis(hono_be_t1850, observed).
narrative_ontology:measurement(hono_be_t1875, honor_settlement_legitimacy__drop_reading, base_extractiveness, 1875, 0.68).
narrative_ontology:measurement_basis(hono_be_t1875, observed).
narrative_ontology:measurement(hono_be_t1900, honor_settlement_legitimacy__drop_reading, base_extractiveness, 1900, 0.68).
narrative_ontology:measurement_basis(hono_be_t1900, observed).

% Suppression requirement over time
narrative_ontology:measurement(hono_su_t1700, honor_settlement_legitimacy__drop_reading, suppression_requirement, 1700, 0.35).
narrative_ontology:measurement_basis(hono_su_t1700, projected).
narrative_ontology:measurement(hono_su_t1750, honor_settlement_legitimacy__drop_reading, suppression_requirement, 1750, 0.48).
narrative_ontology:measurement_basis(hono_su_t1750, observed).
narrative_ontology:measurement(hono_su_t1800, honor_settlement_legitimacy__drop_reading, suppression_requirement, 1800, 0.58).
narrative_ontology:measurement_basis(hono_su_t1800, observed).
narrative_ontology:measurement(hono_su_t1850, honor_settlement_legitimacy__drop_reading, suppression_requirement, 1850, 0.68).
narrative_ontology:measurement_basis(hono_su_t1850, observed).
narrative_ontology:measurement(hono_su_t1875, honor_settlement_legitimacy__drop_reading, suppression_requirement, 1875, 0.7).
narrative_ontology:measurement_basis(hono_su_t1875, observed).
narrative_ontology:measurement(hono_su_t1900, honor_settlement_legitimacy__drop_reading, suppression_requirement, 1900, 0.72).
narrative_ontology:measurement_basis(hono_su_t1900, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=1700, tn=1900
narrative_ontology:measurement(hono_grid_01, honor_settlement_legitimacy__drop_reading, accessibility_collapse(class), 1700, 0.48).
narrative_ontology:measurement(hono_grid_02, honor_settlement_legitimacy__drop_reading, accessibility_collapse(class), 1900, 0.65).
narrative_ontology:measurement(hono_grid_03, honor_settlement_legitimacy__drop_reading, accessibility_collapse(individual), 1700, 0.62).
narrative_ontology:measurement(hono_grid_04, honor_settlement_legitimacy__drop_reading, accessibility_collapse(individual), 1900, 0.58).
narrative_ontology:measurement(hono_grid_05, honor_settlement_legitimacy__drop_reading, accessibility_collapse(organizational), 1700, 0.55).
narrative_ontology:measurement(hono_grid_06, honor_settlement_legitimacy__drop_reading, accessibility_collapse(organizational), 1900, 0.72).
narrative_ontology:measurement(hono_grid_07, honor_settlement_legitimacy__drop_reading, accessibility_collapse(structural), 1700, 0.42).
narrative_ontology:measurement(hono_grid_08, honor_settlement_legitimacy__drop_reading, accessibility_collapse(structural), 1900, 0.68).
narrative_ontology:measurement(hono_grid_09, honor_settlement_legitimacy__drop_reading, resistance(class), 1700, 0.75).
narrative_ontology:measurement(hono_grid_10, honor_settlement_legitimacy__drop_reading, resistance(class), 1900, 0.5).
narrative_ontology:measurement(hono_grid_11, honor_settlement_legitimacy__drop_reading, resistance(individual), 1700, 0.7).
narrative_ontology:measurement(hono_grid_12, honor_settlement_legitimacy__drop_reading, resistance(individual), 1900, 0.62).
narrative_ontology:measurement(hono_grid_13, honor_settlement_legitimacy__drop_reading, resistance(organizational), 1700, 0.68).
narrative_ontology:measurement(hono_grid_14, honor_settlement_legitimacy__drop_reading, resistance(organizational), 1900, 0.42).
narrative_ontology:measurement(hono_grid_15, honor_settlement_legitimacy__drop_reading, resistance(structural), 1700, 0.72).
narrative_ontology:measurement(hono_grid_16, honor_settlement_legitimacy__drop_reading, resistance(structural), 1900, 0.48).
narrative_ontology:measurement(hono_grid_17, honor_settlement_legitimacy__drop_reading, stakes_inflation(class), 1700, 0.45).
narrative_ontology:measurement(hono_grid_18, honor_settlement_legitimacy__drop_reading, stakes_inflation(class), 1900, 0.62).
narrative_ontology:measurement(hono_grid_19, honor_settlement_legitimacy__drop_reading, stakes_inflation(individual), 1700, 0.65).
narrative_ontology:measurement(hono_grid_20, honor_settlement_legitimacy__drop_reading, stakes_inflation(individual), 1900, 0.58).
narrative_ontology:measurement(hono_grid_21, honor_settlement_legitimacy__drop_reading, stakes_inflation(organizational), 1700, 0.52).
narrative_ontology:measurement(hono_grid_22, honor_settlement_legitimacy__drop_reading, stakes_inflation(organizational), 1900, 0.68).
narrative_ontology:measurement(hono_grid_23, honor_settlement_legitimacy__drop_reading, stakes_inflation(structural), 1700, 0.38).
narrative_ontology:measurement(hono_grid_24, honor_settlement_legitimacy__drop_reading, stakes_inflation(structural), 1900, 0.55).
narrative_ontology:measurement(hono_grid_25, honor_settlement_legitimacy__drop_reading, suppression(class), 1700, 0.4).
narrative_ontology:measurement(hono_grid_26, honor_settlement_legitimacy__drop_reading, suppression(class), 1900, 0.72).
narrative_ontology:measurement(hono_grid_27, honor_settlement_legitimacy__drop_reading, suppression(individual), 1700, 0.55).
narrative_ontology:measurement(hono_grid_28, honor_settlement_legitimacy__drop_reading, suppression(individual), 1900, 0.68).
narrative_ontology:measurement(hono_grid_29, honor_settlement_legitimacy__drop_reading, suppression(organizational), 1700, 0.32).
narrative_ontology:measurement(hono_grid_30, honor_settlement_legitimacy__drop_reading, suppression(organizational), 1900, 0.75).
narrative_ontology:measurement(hono_grid_31, honor_settlement_legitimacy__drop_reading, suppression(structural), 1700, 0.25).
narrative_ontology:measurement(hono_grid_32, honor_settlement_legitimacy__drop_reading, suppression(structural), 1900, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_settlement_legitimacy__drop_reading, attachment_coordination).
narrative_ontology:boltzmann_floor_override(honor_settlement_legitimacy__drop_reading, 0.12).
narrative_ontology:affects_constraint(honor_settlement_legitimacy__drop_reading, honor_settlement_legitimacy__contraction_reading).
narrative_ontology:affects_constraint(honor_settlement_legitimacy__drop_reading, honor_settlement_legitimacy__composite_reading).

% DUAL FORMULATION NOTE:
% This constraint is part of the honor_settlement_legitimacy kernel family. The drop_reading (this file) claims that honor culture persists as a cognitively live option in residual communities, organizing dueling as a legitimate settlement mechanism despite state suppression. The contraction_reading treats the same historical phenomenon but frames it as cognitive transformation—honor becomes unthinkable. The composite_reading frames decline as overdetermined by multiple suppression mechanisms. All three readings share the same referent (the historical decline of dueling) and the same interval (1700–1900), but they instantiate structurally distinct normative claims about how and why persistence occurs. The ε value differs across readings because each reading's own logic instantiates different extractiveness: the drop_reading's ε (0.68) reflects the constraint as a persistent settlement system increasingly masked by theater; the contraction_reading's ε would be lower because the cognitive transformation narrativizes the decline as inevitable and natural; the composite_reading's ε would assess overdetermined mechanisms differently. These are not different measurements of one thing—they are different constraints grounded in different reading commitments (OQ-26; ε-invariance principle). All three must be present in the corpus for the kernel contest to be analytically visible.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
