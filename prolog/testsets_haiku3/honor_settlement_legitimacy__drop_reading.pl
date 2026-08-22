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
    narrative_ontology:constraint_vindicates/2,
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
 *   human_readable: Honor-Settlement Legitimacy (Drop Reading): Residual Dueling Practice
 *   domain: legal/cultural/social
 *
 * SUMMARY:
 *   This constraint is the DROP READING of the contested kernel
 *   honor_settlement_legitimacy. It models dueling as a persisting fringe
 *   practice within residual honor culture communities despite comprehensive
 *   state legal prohibition. Under this reading, honor settlement remains a
 *   live normative option—not merely as historical memory or theoretical
 *   position, but as an actually-operative dispute resolution mechanism in
 *   specific geographic/social pockets (rural gentry, military castes,
 *   isolated communities). The constraint extracts legitimacy from the state
 *   legal monopoly: the state must continuously defend its claim to exclusive
 *   authority over violence settlement precisely because honor culture
 *   communities refuse to cede jurisdiction. This is not a constraint that
 *   has atrophied (that is the piton reading)—it is a constraint that
 *   persists because multiple parties actively maintain it: honor communities
 *   maintain honor settlement as normatively binding, the state maintains
 *   suppressive enforcement machinery, and potential participants maintain
 *   identity-locked positions inside honor communities despite the legal
 *   costs.
 *
 * KEY AGENTS:
 *   - honor_culture_communities: Maintain honor settlement as live practice; benefit from legitimacy of internal resolution; bear identity-lock that makes exit impossible without social death.
 *   - state_legal_monopoly: Pays the cost of continuous enforcement against residual honor settlement; bears the extraction of legitimacy loss as honor culture persists.
 *   - potential_dueling_participants: Trapped between honor norms (requiring acceptance of challenges) and state law (criminalizing participation); bear both death risk and legal prosecution risk simultaneously.
 *   - state_enforcement_apparatus: Observes and records regional variance in dueling persistence and prosecution patterns.
 *   - rival_justice_frameworks: Excluded from formal recognition; persist as underground normative repertoire.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_settlement_legitimacy__drop_reading, 0.62).
domain_priors:suppression_score(honor_settlement_legitimacy__drop_reading, 0.71).
domain_priors:theater_ratio(honor_settlement_legitimacy__drop_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_settlement_legitimacy__drop_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(honor_settlement_legitimacy__drop_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(honor_settlement_legitimacy__drop_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_settlement_legitimacy__drop_reading, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(honor_settlement_legitimacy__drop_reading, resistance, 0.59).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_settlement_legitimacy__drop_reading, tangled_rope).
narrative_ontology:human_readable(honor_settlement_legitimacy__drop_reading, "Honor-Settlement Legitimacy (Drop Reading): Residual Dueling Practice").
narrative_ontology:topic_domain(honor_settlement_legitimacy__drop_reading, "legal/cultural/social").

domain_priors:requires_active_enforcement(honor_settlement_legitimacy__drop_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_settlement_legitimacy__drop_reading, 'fc0da942-0192-4998-a3f3-123af1054fca').
narrative_ontology:cs_kernel_codification('fc0da942-0192-4998-a3f3-123af1054fca', distributed).
narrative_ontology:cs_authority_grounding('fc0da942-0192-4998-a3f3-123af1054fca', lineage).
narrative_ontology:cs_interpretation_layer_present('fc0da942-0192-4998-a3f3-123af1054fca').
narrative_ontology:cs_reading_relation('fc0da942-0192-4998-a3f3-123af1054fca', honor_settlement_legitimacy__contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('fc0da942-0192-4998-a3f3-123af1054fca', honor_settlement_legitimacy__composite_reading, influences).
narrative_ontology:cs_axiom('fc0da942-0192-4998-a3f3-123af1054fca', foundational, honor_settlement_remains_live_normative_option).
narrative_ontology:cs_axiom_status(honor_settlement_remains_live_normative_option, holdable).
narrative_ontology:cs_axiom_grounding('fc0da942-0192-4998-a3f3-123af1054fca', honor_settlement_remains_live_normative_option, deontological).
narrative_ontology:cs_axiom('fc0da942-0192-4998-a3f3-123af1054fca', foundational, community_authority_superior_to_state_for_honor_disputes).
narrative_ontology:cs_axiom_status(community_authority_superior_to_state_for_honor_disputes, holdable).
narrative_ontology:cs_axiom_grounding('fc0da942-0192-4998-a3f3-123af1054fca', community_authority_superior_to_state_for_honor_disputes, conventional).
narrative_ontology:cs_reference_frame('fc0da942-0192-4998-a3f3-123af1054fca', honor_community_epistemic_supremacy).
narrative_ontology:cs_drift_state('fc0da942-0192-4998-a3f3-123af1054fca', contemporary_liberal_legal_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('fc0da942-0192-4998-a3f3-123af1054fca', '').
narrative_ontology:cs_kernel_id(honor_settlement_legitimacy__drop_reading, honor_settlement_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_settlement_legitimacy__drop_reading, honor_culture_communities).
narrative_ontology:constraint_victim(honor_settlement_legitimacy__drop_reading, state_legal_monopoly).
narrative_ontology:constraint_victim(honor_settlement_legitimacy__drop_reading, potential_dueling_participants).
narrative_ontology:constraint_vindicates(honor_settlement_legitimacy__drop_reading, honor_culture_survival_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Residual adherents maintain honor settlement practices as normative resolution mechanism within tight geographic/social niches (rural gentry, military castes, isolated communities). They continue to regard dueling as legitimate dispute resolution for matters of personal honor despite criminal prohibition. Their collective identity fuses with honor-culture practices; exit from these communities means social death. They administer honor disputes internally and interpret state law as inapplicable to their affairs of honor.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__drop_reading, honor_culture_communities, agenda_setter,
    moderate, generational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(honor_settlement_legitimacy__drop_reading, honor_culture_communities, beneficiary).

% Bears the cost of maintaining legal monopoly on violence settlement while residual honor-settlement persists outside state jurisdiction. Must continuously prosecute dueling incidents, maintain enforcement machinery in jurisdictions where dueling remains culturally live, and defend the principle that all disputes flow through state courts. The constraint persists precisely because state enforcement is incomplete: honor culture survives in structural pockets where state reach is weak or where social cohesion is stronger than legal authority.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__drop_reading, state_legal_monopoly, payer,
    institutional, generational, constrained, national).

% Embedded in honor communities where they may face dueling challenges as enforcement of honor norms. Exit requires leaving the community, which dissolves their entire social identity, economic standing, and kinship networks. They bear the risk of legal prosecution if they accept or issue challenges, yet face social death and honor degradation if they refuse. Their choices are trapped between state law (prosecution) and community norms (honor requirement).
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__drop_reading, potential_dueling_participants, payer,
    moderate, biographical, identity_locked, regional).

% Maintains records of dueling incidents, prosecutions, and regional persistence patterns. Observes that dueling prosecution rates cluster in specific geographic regions and within identifiable social strata. Enforcement is selective and variable because complete suppression of all honor practices would require surveillance and intervention incompatible with liberal legal regimes that prioritize individual privacy and local autonomy.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__drop_reading, state_enforcement_apparatus, observer,
    institutional, generational, analytical, national).

% Honor-resolution traditions that would legitimize dueling as dispute settlement are structurally excluded from formal legal recognition. They have no voice in state law, no formal standing, no institutional seat at the negotiation table. Their exclusion is what state legal monopoly means: alternative frameworks are driven underground but persist in social practice.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__drop_reading, rival_justice_frameworks, excluded,
    powerless, biographical, trapped, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(honor_settlement_legitimacy__drop_reading, honor_culture_communities).
narrative_ontology:fixing_cost_class(honor_settlement_legitimacy__drop_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Settles disputes about personal honor and social status within communities where state law is regarded as inapplicable or inferior to honor norms. Provides rapid, decisive resolution that community members regard as legitimate, without requiring appeal to external (state) authority.
% TRANSFER_FUNCTION: Transfers legitimacy from state legal monopoly back to honor community authority; transfers risk of death or serious injury to participants; transfers enforcement costs to the state (which must prosecute violations); redistributes social status within honor communities through honor/shame mechanisms.
% ABSENT_VOICES: Legal scholars who defend honor-culture legitimacy remain excluded from mainstream legal discourse; communities maintaining honor practices lack formal representation in state legislatures; persons who have been driven out of honor communities by refusing dueling participate only as external critics, never as seated parties; rival justice frameworks (customary law, honor councils) are excluded from formal recognition.
% DISAPPEARANCE_RATIONALE: From the state legal perspective, if dueling disappeared (honor settlement suppressed completely), the arrangement would vanish and legal uniformity would hold. From honor communities' perspective, if dueling disappeared, the constraint—the suppression itself—would vanish, but the underlying honor culture might persist in attenuated form (reputation management, estrangement, informal status degradation without lethal force). The disappearance question asks which thing disappears: the constraint (dueling) or the enforcing pressure (state legal monopoly).
% FOUNDING_PROBLEM: Personal honor claims require credible, community-recognized settlement mechanisms. In pre-state, decentralized societies, dueling provided a decisive mechanism that honor communities themselves recognized as legitimate. When state legal systems emerged claiming monopoly on violence, they displaced but did not eliminate honor settlement practices.
% FOUNDING_PROBLEM_CORROBORATION: Honor communities attest the founding problem remains live: honor disputes cannot be resolved by state courts, which treat honor as non-justiciable or as irrational passion rather than legitimate legal interest. State legal scholars and prosecutors attest the founding problem is dead: modern law has displaced honor as a basis for legitimate violence. Historians and anthropologists (outside both camps) attest the founding problem is CONTENTIOUSLY alive—that honor settlement persists as a live practice and normative reference point in residual communities.
narrative_ontology:disappearance_verdict(honor_settlement_legitimacy__drop_reading, contested).
narrative_ontology:founding_problem_status(honor_settlement_legitimacy__drop_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_settlement_legitimacy__drop_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(honor_settlement_legitimacy__drop_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_settlement_legitimacy__drop_reading, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is moderate-high (0.62) because the constraint operates as a transfer of legitimacy from state legal authority to honor community authority within protected social space. Suppression is elevated (0.71) because the state must maintain active enforcement machinery—dueling is not naturally suppressed by market forces or social evolution; it is actively policed. Theater ratio is moderate (0.48) because enforcement serves both a genuine protective function (preventing death) and an expressive function (state demonstrating its monopoly). Accessibility of alternatives is LOW (0.38) because for agents embedded in honor communities, alternatives to honor settlement are genuinely unavailable without exit, which means social death. The coercion grid shows intensification over the interval: suppression rises from 0.18 (structural) to 0.71 (structural) as state enforcement machinery hardens; resistance falls from 0.72 to 0.42 as honor communities become progressively isolated and their social position weakens; stakes inflation rises from 0.28 to 0.52 as the legal penalties for dueling increase and the social cost of honor degradation becomes sharper. All metrics flow through one shared time grid: 1750, 1800, 1850, 1900, 1925, 1950.
 *
 * PERSPECTIVAL GAP:
 *   The drop reading creates radical seat divergence: from the honor community perspective, dueling is a coordination mechanism solving the founding problem (legitimate honor settlement without state interference); from the state perspective, it is an extractive evasion of legal monopoly. From the state perspective the constraint is (at minimum) a snare imposed on participants who are coerced into honor communities and trapped by identity. From the honor community perspective, the constraint is the STATE'S suppression, which extracts legitimacy and imposes external costs. The engine will compute these differently per seat. The reading adheres to this divergence and does not try to reconcile it.
 *
 * DIRECTIONALITY LOGIC:
 *   Honor communities are structural beneficiaries (they collect the arrangement's core benefit—legitimate internal justice—and control its operation). They are agenda-setters: they set honor norms, interpret honor violations, and administer honor disputes. The state is structurally a payer: it bears enforcement costs and legitimacy loss. Potential participants are deeply asymmetric: they are identity-locked into honor communities (exit is social death), so they bear the constraint's costs (legal risk, death risk, forced participation) without collecting benefits in the sense that agenda-setters do. The state faces constrained exit too: withdrawal of enforcement would cede the monopoly on violence. Directionality for honor communities is LOW (near beneficiary end, d≈0.2); for state institutions it is MODERATE-HIGH (d≈0.7); for potential participants it is HIGH (d≈0.8, constrained exit + asymmetric stakes).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (legitimate honor settlement) is live under this reading: honor communities continue to regard it as unsolved because state law offers no legitimate avenue for honor disputes. The constraint's mandate has NOT died—it is actively maintained by honor community members who treat honor settlement as a live normative requirement. However, the broader institutional context shows mandatrophy pressures: state law has developed alternative mechanisms (tort, reputation, institutional remedies) that honor communities increasingly reject as inadequate for honor disputes. This is the tension the drop reading models: mandatrophy is partial and contested. The constraint persists precisely because some parties actively maintain it (honor communities) while others actively suppress it (state). If mandatrophy were complete (the founding problem dead and universally recognized as dead), the constraint would convert to a pure piton. Under the drop reading, it remains tangled_rope: both coordination function (honor settlement for honor communities) and asymmetric extraction (state enforcement costs, participant death risk) persist simultaneously.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    honor_culture_persistence_mechanism,
    'Is dueling maintained by active defense of honor norms within communities, or by inertial survival in geographic/social isolation?',
    'Ethnographic study of honor communities distinguishing active norm transmission (apprenticeship, socialization) from inertial persistence (lack of assimilation pressure). Compare communities with strong institutional gatekeeping vs. those with weak institutional gatekeeping.',
    'If actively defended, dueling remains a live normative practice and the constraint persists as tangled rope. If inertial, dueling becomes piton (persistent performance without genuine function). This reading assumes active defense; if empirics show inertia, reclassify to piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(honor_culture_persistence_mechanism, empirical, 'Whether honor settlement persists through active norm maintenance or institutional inertia.').

omega_variable(
    suppression_internalization_asymmetry,
    'Are participants trapped by structural suppression (legal enforcement, geographic isolation, economic dependency on the community) or by internalized suppression (they have internalized honor norms as their own and believe exit is literally unthinkable)?',
    'Post-exit ethnographic interviews with people who left honor communities: do they maintain belief in honor settlement as legitimate (internalized) or explicitly reject it (externally suppressed)?',
    'If structural suppression: fixing the constraint requires changing state enforcement or community gatekeeping. If internalized: fixing requires cognitive reframing, which is slower and more difficult. Mixed: some participants are structurally trapped, others are cognitively trapped.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_internalization_asymmetry, empirical, 'Structural vs. internalized suppression in identity-locked participants.').

omega_variable(
    reading_relatedness_to_siblings,
    'Is the drop reading''s claim (dueling persists as live practice) logically compatible with the contraction reading''s claim (dueling became cognitively unthinkable), or do these readings foreclose each other?',
    'Analytically: if dueling is cognitively unthinkable in the contraction reading but remains a live normative option in the drop reading, what has changed? Either cognitive unthinkability is partial/regional (the readings coexist), or one reading is wrong about the historical trajectory.',
    'If readings foreclose each other: only one is true and the kernel resolves in favor of one. If they coexist: dueling is cognitively unthinkable for mainstream populations but remains live in honor communities—readings occupy different social constituencies. This omega documents the kernel''s internal structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_relatedness_to_siblings, conceptual, 'Whether drop and contraction readings are logically compatible or mutually foreclosing.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_settlement_legitimacy__drop_reading, 1750, 1950).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hono_tr_t1750, honor_settlement_legitimacy__drop_reading, theater_ratio, 1750, 0.22).
narrative_ontology:measurement_basis(hono_tr_t1750, projected).
narrative_ontology:measurement(hono_tr_t1800, honor_settlement_legitimacy__drop_reading, theater_ratio, 1800, 0.26).
narrative_ontology:measurement_basis(hono_tr_t1800, observed).
narrative_ontology:measurement(hono_tr_t1850, honor_settlement_legitimacy__drop_reading, theater_ratio, 1850, 0.32).
narrative_ontology:measurement_basis(hono_tr_t1850, observed).
narrative_ontology:measurement(hono_tr_t1900, honor_settlement_legitimacy__drop_reading, theater_ratio, 1900, 0.43).
narrative_ontology:measurement_basis(hono_tr_t1900, observed).
narrative_ontology:measurement(hono_tr_t1925, honor_settlement_legitimacy__drop_reading, theater_ratio, 1925, 0.46).
narrative_ontology:measurement_basis(hono_tr_t1925, observed).
narrative_ontology:measurement(hono_tr_t1950, honor_settlement_legitimacy__drop_reading, theater_ratio, 1950, 0.48).
narrative_ontology:measurement_basis(hono_tr_t1950, observed).

% Extraction over time
narrative_ontology:measurement(hono_be_t1750, honor_settlement_legitimacy__drop_reading, base_extractiveness, 1750, 0.48).
narrative_ontology:measurement_basis(hono_be_t1750, projected).
narrative_ontology:measurement(hono_be_t1800, honor_settlement_legitimacy__drop_reading, base_extractiveness, 1800, 0.52).
narrative_ontology:measurement_basis(hono_be_t1800, observed).
narrative_ontology:measurement(hono_be_t1850, honor_settlement_legitimacy__drop_reading, base_extractiveness, 1850, 0.58).
narrative_ontology:measurement_basis(hono_be_t1850, observed).
narrative_ontology:measurement(hono_be_t1900, honor_settlement_legitimacy__drop_reading, base_extractiveness, 1900, 0.62).
narrative_ontology:measurement_basis(hono_be_t1900, observed).
narrative_ontology:measurement(hono_be_t1925, honor_settlement_legitimacy__drop_reading, base_extractiveness, 1925, 0.63).
narrative_ontology:measurement_basis(hono_be_t1925, observed).
narrative_ontology:measurement(hono_be_t1950, honor_settlement_legitimacy__drop_reading, base_extractiveness, 1950, 0.62).
narrative_ontology:measurement_basis(hono_be_t1950, observed).

% Suppression requirement over time
narrative_ontology:measurement(hono_su_t1750, honor_settlement_legitimacy__drop_reading, suppression_requirement, 1750, 0.35).
narrative_ontology:measurement_basis(hono_su_t1750, projected).
narrative_ontology:measurement(hono_su_t1800, honor_settlement_legitimacy__drop_reading, suppression_requirement, 1800, 0.42).
narrative_ontology:measurement_basis(hono_su_t1800, observed).
narrative_ontology:measurement(hono_su_t1850, honor_settlement_legitimacy__drop_reading, suppression_requirement, 1850, 0.55).
narrative_ontology:measurement_basis(hono_su_t1850, observed).
narrative_ontology:measurement(hono_su_t1900, honor_settlement_legitimacy__drop_reading, suppression_requirement, 1900, 0.68).
narrative_ontology:measurement_basis(hono_su_t1900, observed).
narrative_ontology:measurement(hono_su_t1925, honor_settlement_legitimacy__drop_reading, suppression_requirement, 1925, 0.7).
narrative_ontology:measurement_basis(hono_su_t1925, observed).
narrative_ontology:measurement(hono_su_t1950, honor_settlement_legitimacy__drop_reading, suppression_requirement, 1950, 0.71).
narrative_ontology:measurement_basis(hono_su_t1950, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=1750, tn=1950
narrative_ontology:measurement(hono_grid_01, honor_settlement_legitimacy__drop_reading, accessibility_collapse(class), 1750, 0.18).
narrative_ontology:measurement(hono_grid_02, honor_settlement_legitimacy__drop_reading, accessibility_collapse(class), 1950, 0.4).
narrative_ontology:measurement(hono_grid_03, honor_settlement_legitimacy__drop_reading, accessibility_collapse(individual), 1750, 0.12).
narrative_ontology:measurement(hono_grid_04, honor_settlement_legitimacy__drop_reading, accessibility_collapse(individual), 1950, 0.38).
narrative_ontology:measurement(hono_grid_05, honor_settlement_legitimacy__drop_reading, accessibility_collapse(organizational), 1750, 0.15).
narrative_ontology:measurement(hono_grid_06, honor_settlement_legitimacy__drop_reading, accessibility_collapse(organizational), 1950, 0.35).
narrative_ontology:measurement(hono_grid_07, honor_settlement_legitimacy__drop_reading, accessibility_collapse(structural), 1750, 0.22).
narrative_ontology:measurement(hono_grid_08, honor_settlement_legitimacy__drop_reading, accessibility_collapse(structural), 1950, 0.38).
narrative_ontology:measurement(hono_grid_09, honor_settlement_legitimacy__drop_reading, resistance(class), 1750, 0.7).
narrative_ontology:measurement(hono_grid_10, honor_settlement_legitimacy__drop_reading, resistance(class), 1950, 0.4).
narrative_ontology:measurement(hono_grid_11, honor_settlement_legitimacy__drop_reading, resistance(individual), 1750, 0.65).
narrative_ontology:measurement(hono_grid_12, honor_settlement_legitimacy__drop_reading, resistance(individual), 1950, 0.35).
narrative_ontology:measurement(hono_grid_13, honor_settlement_legitimacy__drop_reading, resistance(organizational), 1750, 0.68).
narrative_ontology:measurement(hono_grid_14, honor_settlement_legitimacy__drop_reading, resistance(organizational), 1950, 0.38).
narrative_ontology:measurement(hono_grid_15, honor_settlement_legitimacy__drop_reading, resistance(structural), 1750, 0.72).
narrative_ontology:measurement(hono_grid_16, honor_settlement_legitimacy__drop_reading, resistance(structural), 1950, 0.42).
narrative_ontology:measurement(hono_grid_17, honor_settlement_legitimacy__drop_reading, stakes_inflation(class), 1750, 0.32).
narrative_ontology:measurement(hono_grid_18, honor_settlement_legitimacy__drop_reading, stakes_inflation(class), 1950, 0.58).
narrative_ontology:measurement(hono_grid_19, honor_settlement_legitimacy__drop_reading, stakes_inflation(individual), 1750, 0.25).
narrative_ontology:measurement(hono_grid_20, honor_settlement_legitimacy__drop_reading, stakes_inflation(individual), 1950, 0.55).
narrative_ontology:measurement(hono_grid_21, honor_settlement_legitimacy__drop_reading, stakes_inflation(organizational), 1750, 0.35).
narrative_ontology:measurement(hono_grid_22, honor_settlement_legitimacy__drop_reading, stakes_inflation(organizational), 1950, 0.61).
narrative_ontology:measurement(hono_grid_23, honor_settlement_legitimacy__drop_reading, stakes_inflation(structural), 1750, 0.28).
narrative_ontology:measurement(hono_grid_24, honor_settlement_legitimacy__drop_reading, stakes_inflation(structural), 1950, 0.52).
narrative_ontology:measurement(hono_grid_25, honor_settlement_legitimacy__drop_reading, suppression(class), 1750, 0.2).
narrative_ontology:measurement(hono_grid_26, honor_settlement_legitimacy__drop_reading, suppression(class), 1950, 0.72).
narrative_ontology:measurement(hono_grid_27, honor_settlement_legitimacy__drop_reading, suppression(individual), 1750, 0.15).
narrative_ontology:measurement(hono_grid_28, honor_settlement_legitimacy__drop_reading, suppression(individual), 1950, 0.73).
narrative_ontology:measurement(hono_grid_29, honor_settlement_legitimacy__drop_reading, suppression(organizational), 1750, 0.22).
narrative_ontology:measurement(hono_grid_30, honor_settlement_legitimacy__drop_reading, suppression(organizational), 1950, 0.68).
narrative_ontology:measurement(hono_grid_31, honor_settlement_legitimacy__drop_reading, suppression(structural), 1750, 0.18).
narrative_ontology:measurement(hono_grid_32, honor_settlement_legitimacy__drop_reading, suppression(structural), 1950, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_settlement_legitimacy__drop_reading, attachment_coordination).
narrative_ontology:boltzmann_floor_override(honor_settlement_legitimacy__drop_reading, 0.12).
narrative_ontology:affects_constraint(honor_settlement_legitimacy__drop_reading, honor_settlement_legitimacy__contraction_reading).
narrative_ontology:affects_constraint(honor_settlement_legitimacy__drop_reading, honor_settlement_legitimacy__composite_reading).

% DUAL FORMULATION NOTE:
% The honor_settlement_legitimacy kernel decomposes into three structurally distinct constraint stories. DROP_READING models dueling as persisting practice in residual niches (tangled rope: coordination + suppression). CONTRACTION_READING models the cognitive transformation that rendered dueling unthinkable for mainstream populations (mountain → rope conversion, or piton). COMPOSITE_READING models overdetermined decline via multiple reinforcing mechanisms. The three readings share a referent (the kernel: honor settlement legitimacy) but assess it from different vantage points within the same time interval. DROP_READING asserts persistence in structural pockets; CONTRACTION_READING asserts cognitive collapse; COMPOSITE_READING asserts causal overdetermination. Each reading instantiates a single constraint (ε-invariant); the kernel contest is routed through their network relationships.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
