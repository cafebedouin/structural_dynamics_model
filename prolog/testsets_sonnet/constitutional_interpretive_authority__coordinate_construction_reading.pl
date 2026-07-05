% ============================================================================
% CONSTRAINT STORY: constitutional_interpretive_authority__coordinate_construction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_interpretive_authority__coordinate_construction_reading, []).

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
 *   constraint_id: constitutional_interpretive_authority__coordinate_construction_reading
 *   human_readable: Coordinate Construction: Dispersed Inter-Branch Interpretive Authority
 *   domain: constitutional_law/political_theory/jurisprudence
 *
 * SUMMARY:
 *   This constraint models one reading of the interpretive-authority kernel:
 *   the coordinate construction (departmentalist) view, under which no branch
 *   of government holds final say over constitutional meaning. Constitutional
 *   meaning instead emerges from sustained contestation among legislature,
 *   executive, and judiciary, resolved through political mechanisms —
 *   elections, appointments, jurisdiction-stripping, override legislation,
 *   constitutional amendment — rather than through a single dispositive
 *   adjudicative act. This is structurally distinct from the
 *   judicial_supremacy_reading (courts as final arbiter) and the
 *   parliamentary_supremacy_reading (legislature as final arbiter): those are
 *   different constraints with different beneficiary/victim structures and
 *   different epsilon values, not different measurements of this one.
 *
 * KEY AGENTS:
 *   - elected_legislators: agenda_setter/beneficiary (institutional/arbitrage) — use override, budget, and amendment power
 *   - executive_officeholders: agenda_setter/beneficiary (institutional/arbitrage) — use enforcement discretion and appointment power
 *   - judiciary: agenda_setter/payer (institutional/constrained) — issues persuasive but non-final rulings
 *   - minority_rights_claimants: payer (powerless/trapped) — bear instability of contingent protections
 *   - litigants_seeking_final_resolution: payer (moderate/constrained) — bear relitigation costs
 *   - political_coalitions_seeking_durable_change: beneficiary (organized/mobile) — exploit multiple veto points
 *   - constitutional_stability_seekers: excluded (powerless/trapped) — no voice in inter-branch dialogue
 *   - constitutional_theorists: observer (analytical/analytical) — study legitimacy of the arrangement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_interpretive_authority__coordinate_construction_reading, 0.31).
domain_priors:suppression_score(constitutional_interpretive_authority__coordinate_construction_reading, 0.28).
domain_priors:theater_ratio(constitutional_interpretive_authority__coordinate_construction_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_interpretive_authority__coordinate_construction_reading, extractiveness, 0.31).
narrative_ontology:constraint_metric(constitutional_interpretive_authority__coordinate_construction_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(constitutional_interpretive_authority__coordinate_construction_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_interpretive_authority__coordinate_construction_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(constitutional_interpretive_authority__coordinate_construction_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_interpretive_authority__coordinate_construction_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_interpretive_authority__coordinate_construction_reading, "Coordinate Construction: Dispersed Inter-Branch Interpretive Authority").
narrative_ontology:topic_domain(constitutional_interpretive_authority__coordinate_construction_reading, "constitutional_law/political_theory/jurisprudence").

domain_priors:requires_active_enforcement(constitutional_interpretive_authority__coordinate_construction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_interpretive_authority__coordinate_construction_reading, 'c55769be-0f7f-41d9-82bb-b45e0cf880a4').
narrative_ontology:cs_kernel_codification('c55769be-0f7f-41d9-82bb-b45e0cf880a4', distributed).
narrative_ontology:cs_authority_grounding('c55769be-0f7f-41d9-82bb-b45e0cf880a4', distributed).
narrative_ontology:cs_reading_relation('c55769be-0f7f-41d9-82bb-b45e0cf880a4', constitutional_interpretive_authority__judicial_supremacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('c55769be-0f7f-41d9-82bb-b45e0cf880a4', constitutional_interpretive_authority__parliamentary_supremacy_reading, coexists_with).
narrative_ontology:cs_axiom('c55769be-0f7f-41d9-82bb-b45e0cf880a4', foundational, no_branch_holds_final_interpretive_say).
narrative_ontology:cs_axiom_status(no_branch_holds_final_interpretive_say, holdable).
narrative_ontology:cs_axiom_grounding('c55769be-0f7f-41d9-82bb-b45e0cf880a4', no_branch_holds_final_interpretive_say, conventional).
narrative_ontology:cs_axiom('c55769be-0f7f-41d9-82bb-b45e0cf880a4', foundational, political_contestation_is_legitimate_settlement_mechanism).
narrative_ontology:cs_axiom_status(political_contestation_is_legitimate_settlement_mechanism, holdable).
narrative_ontology:cs_axiom_grounding('c55769be-0f7f-41d9-82bb-b45e0cf880a4', political_contestation_is_legitimate_settlement_mechanism, instrumental).
narrative_ontology:cs_reference_frame('c55769be-0f7f-41d9-82bb-b45e0cf880a4', departmentalist_founding_ambiguity).
narrative_ontology:cs_drift_state('c55769be-0f7f-41d9-82bb-b45e0cf880a4', contemporary_judicial_review_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c55769be-0f7f-41d9-82bb-b45e0cf880a4', '').
narrative_ontology:cs_kernel_id(constitutional_interpretive_authority__coordinate_construction_reading, constitutional_interpretive_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_interpretive_authority__coordinate_construction_reading, elected_legislators).
narrative_ontology:constraint_beneficiary(constitutional_interpretive_authority__coordinate_construction_reading, executive_officeholders).
narrative_ontology:constraint_beneficiary(constitutional_interpretive_authority__coordinate_construction_reading, political_coalitions_seeking_durable_change).
narrative_ontology:constraint_victim(constitutional_interpretive_authority__coordinate_construction_reading, minority_rights_claimants).
narrative_ontology:constraint_victim(constitutional_interpretive_authority__coordinate_construction_reading, litigants_seeking_final_resolution).
narrative_ontology:constraint_victim(constitutional_interpretive_authority__coordinate_construction_reading, constitutional_stability_seekers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(constitutional_interpretive_authority__coordinate_construction_reading, judiciary).
narrative_ontology:constraint_vindicates(constitutional_interpretive_authority__coordinate_construction_reading, departmentalism).
narrative_ontology:constraint_vindicates(constitutional_interpretive_authority__coordinate_construction_reading, popular_constitutionalism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Legislate constitutional meaning through statute, override attempts, court-packing threats, jurisdiction-stripping proposals, and constitutional amendment. Benefits from a system where no adverse judicial ruling is truly final — every unfavorable interpretation can be contested through appointments, budget riders, or amendment campaigns. Bears the cost of never having settled law to build durable policy on.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__coordinate_construction_reading, elected_legislators, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(constitutional_interpretive_authority__coordinate_construction_reading, elected_legislators, beneficiary).

% Issue interpretations through enforcement discretion, signing statements, agency rulemaking, and appointment power over the judiciary itself. Treats disfavored judicial rulings as one input among several rather than binding commands, especially where enforcement machinery is executive-controlled. Gains flexibility; loses a reliable shield against legislative or judicial reversal of executive action.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__coordinate_construction_reading, executive_officeholders, agenda_setter,
    institutional, biographical, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(constitutional_interpretive_authority__coordinate_construction_reading, executive_officeholders, beneficiary).

% Issues rulings that carry persuasive but not final weight — subject to legislative override attempts, appointment-driven doctrinal reversal, and executive non-enforcement. Participates in the dialogue as one voice, not the last word. Its interpretive authority is contingent on continued political deference it cannot compel.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__coordinate_construction_reading, judiciary, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(constitutional_interpretive_authority__coordinate_construction_reading, judiciary, payer).

% Seek constitutional protection against majoritarian action. A favorable court ruling offers no durable security if it can be legislatively circumvented, executively unenforced, or reversed by future court composition shaped by electoral majorities. Bears the cost of an interpretive regime where their protections are only as stable as the current political coalition.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__coordinate_construction_reading, minority_rights_claimants, payer,
    powerless, biographical, trapped, national).

% Bring cases expecting adjudication to settle their dispute. Under coordinate construction, a favorable ruling may be relitigated through political channels rather than treated as dispositive, prolonging uncertainty and litigation cost. Cannot exit the system; must absorb the instability as a cost of doing business with constitutional questions.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__coordinate_construction_reading, litigants_seeking_final_resolution, payer,
    moderate, biographical, constrained, national).

% Use the dispersed-authority structure strategically: when courts are hostile, they pursue legislative override, appointment capture, or amendment; when legislatures are hostile, they pursue litigation. The multiplicity of veto/override points gives well-organized, well-resourced coalitions many paths to eventual victory that under-resourced groups lack.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__coordinate_construction_reading, political_coalitions_seeking_durable_change, beneficiary,
    organized, generational, mobile, national).

% Ordinary citizens, businesses, and institutions that need to plan around settled constitutional rules — property arrangements, contracts premised on regulatory authority, reliance interests built on prior rulings. Have no voice in the inter-branch contest itself; their preference for stability is not a party to the dialogue between the branches.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__coordinate_construction_reading, constitutional_stability_seekers, excluded,
    powerless, civilizational, trapped, national).

% Study and debate whether coordinate construction produces legitimate democratic contestation or unpredictable power struggles dressed as principle. Their analysis shapes which reading (coordinate, judicial supremacy, parliamentary supremacy) gains institutional traction at a given historical moment.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__coordinate_construction_reading, constitutional_theorists, observer,
    analytical, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(constitutional_interpretive_authority__coordinate_construction_reading, diffuse).
narrative_ontology:fixing_cost_class(constitutional_interpretive_authority__coordinate_construction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves the genuine problem of who gets the last word on constitutional meaning by refusing to vest it anywhere permanently — instead routing interpretive disagreement through political mechanisms (elections, appointments, amendment, budget control, override legislation) that require sustained coalition-building rather than a single adjudicative act.
% TRANSFER_FUNCTION: Moves interpretive finality away from any single institution and toward whichever coalition can sustain political mobilization across multiple veto points over time; this systematically favors organized, resourced, patient actors over diffuse or powerless claimants who need a single favorable ruling to be dispositive.
% ABSENT_VOICES: Minority rights claimants and ordinary reliance-interest holders have no seat in the inter-branch dialogue itself — the 'dialogue' is between branches of government, not between the government and the people whose rights or expectations are being redefined. Constitutional stability seekers are structurally outside the conversation entirely.
% DISAPPEARANCE_RATIONALE: If coordinate construction were displaced overnight by a settled hierarchy (either judicial or parliamentary supremacy), the entire strategic landscape for constitutional advocacy would collapse into a single channel: litigation-only or legislation-only. Coalitions currently pursuing multi-front strategies would lose most of their leverage points, and previously unstable rulings would either become permanently binding or permanently revisable by simple majority — a fundamental reorganization of political practice.
% FOUNDING_PROBLEM: Written constitutions do not specify who resolves disputes about their own meaning; early republican designers deliberately left interpretive authority ambiguous partly by omission and partly by design, anticipating that no single branch should be trusted with unchecked final say over the document constituting all three.
% FOUNDING_PROBLEM_CORROBORATION: Political scientists and comparative constitutionalists outside any single branch (e.g., studies of departmentalism in the early U.S. republic, court-curbing literature) attest the ambiguity was structural and partly intentional, not merely a drafting gap. Legislators and executives who benefit from interpretive flexibility assert the same intentionality, which weakens their corroboration value as self-interested parties; independent historical and comparative-law scholarship is the source treated as corroborating from outside the beneficiary set.
narrative_ontology:disappearance_verdict(constitutional_interpretive_authority__coordinate_construction_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_interpretive_authority__coordinate_construction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_interpretive_authority__coordinate_construction_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(constitutional_interpretive_authority__coordinate_construction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_interpretive_authority__coordinate_construction_reading, 0.31, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_interpretive_authority__coordinate_construction_reading_tests).
:- end_tests(constitutional_interpretive_authority__coordinate_construction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored moderate (0.31) and mildly rising over the interval: coordinate construction is not primarily an extraction mechanism, but organized coalitions with resources to sustain multi-front contestation (litigation plus legislative plus appointment strategy) accrue durable advantage over diffuse or resource-poor claimants, and this advantage compounds as strategic sophistication about exploiting multiple veto points increases over time. Suppression is authored low-moderate (0.28): the arrangement does not coerce compliance through force, but it does suppress the possibility of dispositive relief for minority claimants by design — a favorable ruling is never quite final. Accessibility collapse is moderate (0.35): political and legal avenues remain formally open, but practical access to sustained multi-front contestation strongly favors organized actors. Resistance is moderate-high (0.55) because the arrangement is actively contested by those who prefer a settled hierarchy (advocates of both judicial and parliamentary supremacy readings push back against interpretive instability).
 *
 * PERSPECTIVAL GAP:
 *   From the seat of elected legislators or executive officeholders, the arrangement reads as legitimate democratic dialogue — a safeguard against unaccountable judicial or unchecked majoritarian power. From the seat of a minority rights claimant who has won in court but faces legislative override or executive non-enforcement, the same structure reads as extraction of finality itself: victory is perpetually contingent, and the contingency is what political coalitions harvest. The engine should compute a materially different type from the payer seats than from the agenda-setter seats given identical structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Elected legislators and executive officeholders are declared beneficiaries because they retain interpretive leverage and multiple channels to resist adverse rulings — their exit option is arbitrage (they can route around unfavorable interpretations through several institutional paths). Minority rights claimants and litigants seeking final resolution are declared victims because they depend on a single dispositive ruling and cannot exit the multi-front contestation game — their structural position is trapped or constrained. Political coalitions with resources are beneficiaries of the structure's proliferation of veto points, independent of ideological direction, because the proliferation itself favors the well-organized over the powerless.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — preventing any single branch from monopolizing constitutional meaning — remains partially live (contested status): the underlying concern about concentrated interpretive power persists as a legitimate design worry, but the arrangement has also become a resource for strategic actors to indefinitely defer settlement on questions where a single answer would be normatively required (e.g., protection of a vulnerable minority). Classifying this as tangled_rope rather than snare or rope avoids two errors: treating dispersed authority as pure extraction (it does solve a genuine problem of concentrated power) and treating it as pure coordination (it does create asymmetric outcomes favoring resourced actors through the same mechanism that disperses authority).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordinate_construction_reading_identity,
    'Is coordinate construction a genuinely distinct, historically instantiated interpretive regime, or is it better understood as the transitional/unstable state between periods of de facto judicial or parliamentary supremacy?',
    'Comparative constitutional-historical analysis of periods where multiple branches have simultaneously and successfully asserted final interpretive authority (e.g. early U.S. departmentalism, post-Marbury contestation, court-packing threats) versus periods where one reading has stabilized as dominant practice for extended periods.',
    'If coordinate construction never stabilizes and always resolves toward one of the sibling readings, it may be better modeled as a scaffold (a transitional condition with an implicit sunset) rather than a standing tangled_rope; if it persists indefinitely as a stable equilibrium, tangled_rope is the more accurate classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordinate_construction_reading_identity, conceptual, 'Whether coordinate construction is a stable regime or a transitional state between the sibling readings.').

omega_variable(
    kernel_sibling_relationship_asymmetry,
    'Does this reading''s dispersal of authority create structural pressure toward one sibling reading (judicial supremacy) more than the other (parliamentary supremacy), given that courts retain a persuasive-but-not-final voice that legislatures and executives must still engage with?',
    'Track which reading successive contested episodes resolve toward across multiple constitutional systems that have operated under coordinate construction (e.g. does contestation more often end in de facto judicial deference or de facto legislative override?).',
    'An asymmetric pull toward judicial supremacy would mean this reading functions partly as a staging ground for judicial_supremacy_reading rather than a fully independent equilibrium reading; this would refine but not eliminate the reading_relations declared below.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_sibling_relationship_asymmetry, empirical, 'Whether coordinate construction structurally favors drift toward judicial or parliamentary supremacy over time.').

omega_variable(
    beneficiary_capture_of_dispersal_norm,
    'Is the normative commitment to dispersed interpretive authority a principled constitutional design choice, or has it become primarily a tool captured by whichever coalition currently controls the most veto points to indefinitely defer unfavorable settlement?',
    'Examine whether coalitions consistently favor coordinate-construction rhetoric only when they lack control of the judiciary, and abandon it in favor of judicial-supremacy rhetoric once they capture the courts (or the reverse pattern with legislative capture).',
    'If rhetorical commitment to coordinate construction tracks strategic position rather than principle, the coordination function is substantially cover for extraction, pushing the classification toward snare; if commitment is stable regardless of current institutional control, the coordination function is more genuine, supporting tangled_rope or even rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_capture_of_dispersal_norm, empirical, 'Whether declared commitment to dispersed authority tracks principle or strategic institutional position.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_interpretive_authority__coordinate_construction_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t0, constitutional_interpretive_authority__coordinate_construction_reading, theater_ratio, 0, 0.14).
narrative_ontology:measurement(cons_tr_t8, constitutional_interpretive_authority__coordinate_construction_reading, theater_ratio, 8, 0.16).
narrative_ontology:measurement(cons_tr_t16, constitutional_interpretive_authority__coordinate_construction_reading, theater_ratio, 16, 0.18).
narrative_ontology:measurement(cons_tr_t24, constitutional_interpretive_authority__coordinate_construction_reading, theater_ratio, 24, 0.19).
narrative_ontology:measurement(cons_tr_t32, constitutional_interpretive_authority__coordinate_construction_reading, theater_ratio, 32, 0.21).
narrative_ontology:measurement(cons_tr_t40, constitutional_interpretive_authority__coordinate_construction_reading, theater_ratio, 40, 0.22).

% Extraction over time
narrative_ontology:measurement(cons_be_t0, constitutional_interpretive_authority__coordinate_construction_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(cons_be_t8, constitutional_interpretive_authority__coordinate_construction_reading, base_extractiveness, 8, 0.25).
narrative_ontology:measurement(cons_be_t16, constitutional_interpretive_authority__coordinate_construction_reading, base_extractiveness, 16, 0.27).
narrative_ontology:measurement(cons_be_t24, constitutional_interpretive_authority__coordinate_construction_reading, base_extractiveness, 24, 0.29).
narrative_ontology:measurement(cons_be_t32, constitutional_interpretive_authority__coordinate_construction_reading, base_extractiveness, 32, 0.3).
narrative_ontology:measurement(cons_be_t40, constitutional_interpretive_authority__coordinate_construction_reading, base_extractiveness, 40, 0.31).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t0, constitutional_interpretive_authority__coordinate_construction_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(cons_su_t8, constitutional_interpretive_authority__coordinate_construction_reading, suppression_requirement, 8, 0.22).
narrative_ontology:measurement(cons_su_t16, constitutional_interpretive_authority__coordinate_construction_reading, suppression_requirement, 16, 0.24).
narrative_ontology:measurement(cons_su_t24, constitutional_interpretive_authority__coordinate_construction_reading, suppression_requirement, 24, 0.25).
narrative_ontology:measurement(cons_su_t32, constitutional_interpretive_authority__coordinate_construction_reading, suppression_requirement, 32, 0.27).
narrative_ontology:measurement(cons_su_t40, constitutional_interpretive_authority__coordinate_construction_reading, suppression_requirement, 40, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_interpretive_authority__coordinate_construction_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(constitutional_interpretive_authority__coordinate_construction_reading, judicial_supremacy_reading).
narrative_ontology:affects_constraint(constitutional_interpretive_authority__coordinate_construction_reading, parliamentary_supremacy_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the constitutional_interpretive_authority kernel. judicial_supremacy_reading and parliamentary_supremacy_reading are separate constraint files with their own epsilon values, beneficiary/victim structures, and classifications — they are not alternative measurements of this constraint but structurally distinct arrangements of interpretive authority. This reading's dispersed-authority structure creates downstream pressure on both siblings: it destabilizes any claim either sibling makes to permanent finality, since the coordinate reading's persistence as a live alternative is itself evidence used by proponents of contestation against both supremacy claims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
