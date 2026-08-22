% ============================================================================
% CONSTRAINT STORY: us_constitution_text__living_constitutionalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_constitution_text__living_constitutionalist_reading, []).

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
 *   constraint_id: us_constitution_text__living_constitutionalist_reading
 *   human_readable: Living Constitution Interpretive Authority (U.S.)
 *   domain: constitutional_law/legal_philosophy
 *
 * SUMMARY:
 *   This constraint models the living constitutionalist reading of U.S.
 *   constitutional authority: the view that constitutional meaning is not
 *   fixed at ratification but evolves with society, allowing courts to
 *   reinterpret principles to address contemporary circumstances. The reading
 *   is one of three contested framings of the same kernel (the Constitution's
 *   text itself); it competes with originalism and legal positivism. This
 *   story instantiates the living constitutionalist frame alone, modeling it
 *   as a tangled rope: it solves a real coordination problem (keeping an
 *   18th-century document relevant to the 21st century) while extracting from
 *   those who lose under adaptive readings (originalist interpretive
 *   authority, state legislative autonomy over culturally contested issues).
 *   Beneficiaries are rights claimants whose circumstances the historical
 *   text does not explicitly name—they gain constitutional protection through
 *   adaptive interpretation. The constraint's extractiveness has risen over
 *   the interval (0.35 to 0.62) as the scope of adaptively-read rights has
 *   expanded, and theater has increased modestly (0.08 to 0.22) as
 *   originalist criticism has accused the judiciary of substituting
 *   contemporary values for textual meaning.
 *
 * KEY AGENTS:
 *   - Federal judiciary: institutional agenda-setter; holds interpretive authority; justifies adaptive reading as necessary for constitutional longevity
 *   - Rights claimants in changed circumstances: beneficiaries; gain access to constitutional protection for issues the framers did not contemplate (e.g., marriage equality, reproductive autonomy)
 *   - Originalist interpreters: institutional payer; their interpretive framework is treated as subordinate or defeated in judicial discourse
 *   - State legislatures: organized payer; face constraint that laws can be invalidated by reinterpreted constitutional principles
 *   - Constitutional text originalists: excluded; would argue living constitutionalism is judicial supremacy, not legitimate interpretation
 *   - Congress: observer; can propose amendments but not easily overrule judicial interpretation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_text__living_constitutionalist_reading, 0.62).
domain_priors:suppression_score(us_constitution_text__living_constitutionalist_reading, 0.28).
domain_priors:theater_ratio(us_constitution_text__living_constitutionalist_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_text__living_constitutionalist_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(us_constitution_text__living_constitutionalist_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(us_constitution_text__living_constitutionalist_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_text__living_constitutionalist_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(us_constitution_text__living_constitutionalist_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_text__living_constitutionalist_reading, tangled_rope).
narrative_ontology:human_readable(us_constitution_text__living_constitutionalist_reading, "Living Constitution Interpretive Authority (U.S.)").
narrative_ontology:topic_domain(us_constitution_text__living_constitutionalist_reading, "constitutional_law/legal_philosophy").

domain_priors:requires_active_enforcement(us_constitution_text__living_constitutionalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_text__living_constitutionalist_reading, 'facf40fc-f44b-42d6-b800-aabb9e9d6b3b').
narrative_ontology:cs_kernel_codification('facf40fc-f44b-42d6-b800-aabb9e9d6b3b', fixed_text).
narrative_ontology:cs_authority_grounding('facf40fc-f44b-42d6-b800-aabb9e9d6b3b', lineage).
narrative_ontology:cs_interpretation_layer_present('facf40fc-f44b-42d6-b800-aabb9e9d6b3b').
narrative_ontology:cs_reading_relation('facf40fc-f44b-42d6-b800-aabb9e9d6b3b', us_constitution_text__originalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('facf40fc-f44b-42d6-b800-aabb9e9d6b3b', us_constitution_text__positivist_reading, coexists_with).
narrative_ontology:cs_axiom('facf40fc-f44b-42d6-b800-aabb9e9d6b3b', foundational, constitutional_principles_transcend_historical_moment).
narrative_ontology:cs_axiom_status(constitutional_principles_transcend_historical_moment, holdable).
narrative_ontology:cs_axiom_grounding('facf40fc-f44b-42d6-b800-aabb9e9d6b3b', constitutional_principles_transcend_historical_moment, deontological).
narrative_ontology:cs_axiom('facf40fc-f44b-42d6-b800-aabb9e9d6b3b', foundational, judicial_discretion_necessary_for_constitutional_application).
narrative_ontology:cs_axiom_status(judicial_discretion_necessary_for_constitutional_application, holdable).
narrative_ontology:cs_axiom_grounding('facf40fc-f44b-42d6-b800-aabb9e9d6b3b', judicial_discretion_necessary_for_constitutional_application, instrumental).
narrative_ontology:cs_reference_frame('facf40fc-f44b-42d6-b800-aabb9e9d6b3b', living_constitutional_authority).
narrative_ontology:cs_drift_state('facf40fc-f44b-42d6-b800-aabb9e9d6b3b', contemporary_legal_realism_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('facf40fc-f44b-42d6-b800-aabb9e9d6b3b', '').
narrative_ontology:cs_kernel_id(us_constitution_text__living_constitutionalist_reading, us_constitution_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_text__living_constitutionalist_reading, rights_claimants_changed_circumstances).
narrative_ontology:constraint_beneficiary(us_constitution_text__living_constitutionalist_reading, federal_judiciary).
narrative_ontology:constraint_victim(us_constitution_text__living_constitutionalist_reading, claims_to_fixed_meaning).
narrative_ontology:constraint_victim(us_constitution_text__living_constitutionalist_reading, state_legislative_autonomy).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(us_constitution_text__living_constitutionalist_reading, originalist_interpreters).
narrative_ontology:constraint_victim(us_constitution_text__living_constitutionalist_reading, state_legislatures).
narrative_ontology:constraint_vindicates(us_constitution_text__living_constitutionalist_reading, constitutional_principles_transcend_historical_moment).
narrative_ontology:constraint_vindicates(us_constitution_text__living_constitutionalist_reading, judicial_adaptation_necessary_for_constitutional_longevity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets constitutional text in light of contemporary circumstances. Holds authority to re-read historical principles as applying to social contexts the framers did not contemplate (digital privacy, internet commerce, modern family structures). Justifies this power as necessary to preserve constitutional principles' relevance across centuries. Collects institutional prestige and docket power from being the final arbiter of meaning.
narrative_ontology:constraint_stakeholder(us_constitution_text__living_constitutionalist_reading, federal_judiciary, agenda_setter,
    institutional, generational, analytical, national).

% Access constitutional protection for claims the framers' historical text does not explicitly recognize: reproductive autonomy, marriage equality, digital privacy, equal protection of non-traditional families. Depend on judicial willingness to adapt principles to recognize their circumstances as within the Constitution's protective scope. Without adaptive interpretation, they would have no constitutional remedy.
narrative_ontology:constraint_stakeholder(us_constitution_text__living_constitutionalist_reading, rights_claimants_changed_circumstances, beneficiary,
    organized, biographical, constrained, national).

% Argue that living constitutionalism empowers judges to impose their contemporary values on the Constitution, displacing democratic choice and the constraint of fixed meaning. Pay the cost of having their interpretive framework treated as a subordinate or defeated position in judicial opinions. Their alternative reading is available but must compete in institutional discourse where the living constitution reading has occupied dominant seats.
narrative_ontology:constraint_stakeholder(us_constitution_text__living_constitutionalist_reading, originalist_interpreters, payer,
    institutional, generational, constrained, national).

% Face the constraint that their legislation can be invalidated by federal courts applying adaptively-read constitutional principles. Laws on abortion, marriage, education, and social policy are subject to judicial reinterpretation as constitutional principles are updated to new social contexts. They cannot exit this relationship with federal constitutional authority.
narrative_ontology:constraint_stakeholder(us_constitution_text__living_constitutionalist_reading, state_legislatures, payer,
    organized, generational, trapped, national).

% Would argue that the constraint should be called 'judicial supremacy over democratic process' rather than legitimate constitutional adaptation. They are not represented in the judiciary's self-account of the living constitution and are structurally excluded from the interpretive authority structure, though they publish competing readings and file amicus briefs.
narrative_ontology:constraint_stakeholder(us_constitution_text__living_constitutionalist_reading, constitutional_text_originalists, excluded,
    moderate, generational, constrained, national).

% Can propose constitutional amendments to overturn or lock in particular readings, but cannot easily overrule judicial interpretation without amendment. Observes the judiciary's adaptive interpretations and occasionally responds with legislative action or rhetoric, but is not a principal in the constraint's operation.
narrative_ontology:constraint_stakeholder(us_constitution_text__living_constitutionalist_reading, congress, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(us_constitution_text__living_constitutionalist_reading, federal_judiciary).
narrative_ontology:fixing_cost_class(us_constitution_text__living_constitutionalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ensures the Constitution remains applicable to radically changed social circumstances (industrial economy to digital economy, segregation-era to internet-era privacy, nuclear family to blended families) without requiring super-majority amendment for each shift. A fixed-meaning constraint would require constant amendment or would render the Constitution dead text for new contexts.
% TRANSFER_FUNCTION: Moves interpretive authority from the historical text's plain meaning toward contemporary judicial judgment about what principles demand in current circumstances. Transfers legitimacy from 'what the framers wrote' to 'what the principles mean now'. Winners in adaptive readings (rights claimants in novel contexts) gain constitutional protection; losers (claims tied to fixed historical meaning, state legislative prerogatives over issues newly deemed constitutionally constrained) lose autonomy.
% ABSENT_VOICES: Originalist interpreters and strict textualists are structurally excluded from the living constitution framework — their core premise (meaning is fixed at ratification) is not represented in the authority structure that adjudicates constitutional meaning. They can file briefs and publish scholarship, but the institutional bias of the judiciary as currently composed treats adaptive reading as legitimate interpretation rather than as a contested frame.
% DISAPPEARANCE_RATIONALE: If living constitutionalism were replaced by pure originalism or textualism, rights recognized under adaptive readings (abortion access, marriage equality, digital privacy) would lose constitutional ground and revert to state-by-state legislation; the federal judiciary would become a constraint on rights-claimants rather than a protector; legislative majorities in conservative states would reshape social policy. The Constitution's meaning would shrink to its explicit historical scope.
% FOUNDING_PROBLEM: How can a document written in 1787 remain the supreme law for a 21st-century polity? How do principles like 'liberty' and 'equal protection' apply to social contexts the framers never imagined?
% FOUNDING_PROBLEM_CORROBORATION: Federal judges, constitutional scholars across the ideological spectrum (even critics), and rights advocates all acknowledge that fixed historical meaning would render the Constitution inadequate to modern life. Originalists dispute the SOLUTION (adaptive reading) but acknowledge the founding problem is live. Academic constitutional law and judicial opinions document the persistent tension.
narrative_ontology:disappearance_verdict(us_constitution_text__living_constitutionalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(us_constitution_text__living_constitutionalist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_text__living_constitutionalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(us_constitution_text__living_constitutionalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(us_constitution_text__living_constitutionalist_reading, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_text__living_constitutionalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_constitution_text__living_constitutionalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(us_constitution_text__living_constitutionalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) is moderate-to-high because the living constitution reading consolidates enormous interpretive power in the federal judiciary and allows it to override state legislative choices on culturally contested issues without amendment. The constraint's legitimacy does not rest on explicit text or historical consensus—it rests on judges' judgment that principles must adapt. Suppression is low (0.28) because the judiciary does not need to coerce acceptance of adaptive readings; the readings are presented as sound constitutional interpretation, and institutional legitimacy carries them. Theater has increased over time (0.08 to 0.22) as originalist criticism has sharpened, forcing the judiciary to justify adaptive readings more carefully, suggesting rising performative activity to defend the practice. Accessibility collapse is moderate (0.45): alternative readings exist and are available in public discourse, but the judiciary's institutional dominance makes originalist alternatives less accessible within the law itself. Resistance is high (0.72) because originalists, conservatives, and state legislatures actively oppose adaptive readings and argue for constraint-based interpretation.
 *
 * PERSPECTIVAL GAP:
 *   The federal judiciary seat computes the living constitution as legitimate, necessary coordination that preserves constitutional relevance; originalist and state legislative seats compute it as judicial power-grab that usurps democratic choice. The engine computes these divergences from the structural data: the judiciary benefits from interpretive authority and consolidates it; originalists lose institutional standing; states lose autonomy. The measured extraction (0.62) is higher from the payer seats than the beneficiary seat would report—rights claimants see protection, states see constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   The judiciary is the agenda-setter (d near 0.0, beneficiary): it sets the interpretive frame, collects institutional prestige, and is rarely overruled. Rights claimants are organized beneficiaries (d near 0.2): they gain protection but depend on judicial willingness to adapt. Originalist interpreters are institutional payers (d near 0.7): their interpretive tradition is treated as defeated or subordinate; they pay the cost of lost institutional authority. State legislatures are organized payers (d near 0.8): trapped by federal constitutional constraint; cannot exit. The constraint's extractiveness is amplified for these payer seats because they face an adaptively-read federal constraint they cannot remedy without constitutional amendment.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint shows no clear sign of mandatrophy. The founding problem (how to keep a historical document relevant to changed circumstances) remains live, and the living constitution reading addresses it structurally. However, theater has increased modestly over time, suggesting rising performative justification of adaptive readings as originalist criticism has mounted. The measurement trajectory shows stabilization at higher extractiveness (plateau from 30 to 50), suggesting the living constitution frame has solidified as the dominant reading within the judiciary, reducing the active work needed to defend it—a sign of institutional maturation rather than decline.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    meaning_vs_power_ambiguity,
    'Is the measured extractiveness (0.62) a feature of adaptive interpretation itself, or a feature of how the judiciary has wielded adaptiveness to consolidate power? Would a conscientiously-applied living constitution that genuinely serves rights claimants have lower extractiveness than a power-consolidating one?',
    'Comparative analysis of how different institutional contexts (state supreme courts, international courts) apply adaptive interpretation: if extractiveness correlates with institutional power consolidation rather than with adaptiveness itself, the ambiguity is resolved in favor of power; if adaptive readings serve documented claimants across contexts with lower extractiveness, the ambiguity is resolved in favor of legitimate flexibility.',
    'If extractiveness is inherent to adaptiveness, living constitutionalism is structurally a tangled rope; if extractiveness is contingent on judicial self-interest, the constraint could be reconfigured to lower extractiveness while preserving flexibility (e.g., sunset clauses, supermajority requirements, legislative check-in).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(meaning_vs_power_ambiguity, empirical, 'Whether extractiveness is essential to adaptive interpretation or contingent on institutional power consolidation.').

omega_variable(
    founding_problem_vs_cover_story,
    'Does adaptive interpretation genuinely serve the founding problem (keeping the Constitution relevant to changed circumstances), or has that problem become a cover story for judges to impose contemporary values that the amendment process would reject?',
    'Audit whether adaptive readings follow predictable principles or whether they track judicial ideology more closely than constitutional text. Compare interpretations that would entrench conservative vs. progressive values: if adaptive readings favor progressive values disproportionately (or vice versa), the cover-story hypothesis gains credibility.',
    'If adaptive interpretation is genuinely principled, it remains a legitimate coordination solution; if it tracks judicial ideology, it becomes pure extraction (snare) dressed as coordination.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(founding_problem_vs_cover_story, conceptual, 'Whether adaptive interpretation is principled or ideologically driven.').

omega_variable(
    constitutive_vs_constructed_reading,
    'Is living constitutionalism a discovery of what the Constitution always meant (the reading''s self-account: principles are timeless and must be reapplied to new contexts), or is it a constructed reading that privileges adaptation over constraint (originalism''s account: adaptive reading is judicial choice, not discovery)?',
    'The distinction is conceptual, not empirical: no data can settle whether interpretation is discovery or construction. The omega resolves through the engine''s committer-frame logic: if the judiciary''s authority rests on treating adaptation as discovery, then the reading''s legitimacy is vulnerable to evidence that adaptation is choice; if the reading explicitly anchors itself in judicial discretion, the vulnerability is lower.',
    'If adaptive reading is presented as discovery but operates as choice, the constraint''s legitimacy is brittle and risks sudden reclassification if the framing shifts; if presented honestly as discretionary, the extractiveness is clearer and the constraint''s type is more stable.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(constitutive_vs_constructed_reading, conceptual, 'Whether adaptive interpretation is epistemically foundational or constitutively chosen.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_text__living_constitutionalist_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_c_tr_t0, us_constitution_text__living_constitutionalist_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(us_c_tr_t0, observed).
narrative_ontology:measurement(us_c_tr_t10, us_constitution_text__living_constitutionalist_reading, theater_ratio, 10, 0.12).
narrative_ontology:measurement_basis(us_c_tr_t10, observed).
narrative_ontology:measurement(us_c_tr_t20, us_constitution_text__living_constitutionalist_reading, theater_ratio, 20, 0.18).
narrative_ontology:measurement_basis(us_c_tr_t20, observed).
narrative_ontology:measurement(us_c_tr_t30, us_constitution_text__living_constitutionalist_reading, theater_ratio, 30, 0.21).
narrative_ontology:measurement_basis(us_c_tr_t30, observed).
narrative_ontology:measurement(us_c_tr_t40, us_constitution_text__living_constitutionalist_reading, theater_ratio, 40, 0.22).
narrative_ontology:measurement_basis(us_c_tr_t40, observed).
narrative_ontology:measurement(us_c_tr_t50, us_constitution_text__living_constitutionalist_reading, theater_ratio, 50, 0.22).
narrative_ontology:measurement_basis(us_c_tr_t50, observed).

% Extraction over time
narrative_ontology:measurement(us_c_be_t0, us_constitution_text__living_constitutionalist_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement_basis(us_c_be_t0, observed).
narrative_ontology:measurement(us_c_be_t10, us_constitution_text__living_constitutionalist_reading, base_extractiveness, 10, 0.48).
narrative_ontology:measurement_basis(us_c_be_t10, observed).
narrative_ontology:measurement(us_c_be_t20, us_constitution_text__living_constitutionalist_reading, base_extractiveness, 20, 0.58).
narrative_ontology:measurement_basis(us_c_be_t20, observed).
narrative_ontology:measurement(us_c_be_t30, us_constitution_text__living_constitutionalist_reading, base_extractiveness, 30, 0.6).
narrative_ontology:measurement_basis(us_c_be_t30, observed).
narrative_ontology:measurement(us_c_be_t40, us_constitution_text__living_constitutionalist_reading, base_extractiveness, 40, 0.62).
narrative_ontology:measurement_basis(us_c_be_t40, observed).
narrative_ontology:measurement(us_c_be_t50, us_constitution_text__living_constitutionalist_reading, base_extractiveness, 50, 0.62).
narrative_ontology:measurement_basis(us_c_be_t50, observed).

% Suppression requirement over time
narrative_ontology:measurement(us_c_su_t0, us_constitution_text__living_constitutionalist_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement_basis(us_c_su_t0, observed).
narrative_ontology:measurement(us_c_su_t10, us_constitution_text__living_constitutionalist_reading, suppression_requirement, 10, 0.2).
narrative_ontology:measurement_basis(us_c_su_t10, observed).
narrative_ontology:measurement(us_c_su_t20, us_constitution_text__living_constitutionalist_reading, suppression_requirement, 20, 0.26).
narrative_ontology:measurement_basis(us_c_su_t20, observed).
narrative_ontology:measurement(us_c_su_t30, us_constitution_text__living_constitutionalist_reading, suppression_requirement, 30, 0.28).
narrative_ontology:measurement_basis(us_c_su_t30, observed).
narrative_ontology:measurement(us_c_su_t40, us_constitution_text__living_constitutionalist_reading, suppression_requirement, 40, 0.28).
narrative_ontology:measurement_basis(us_c_su_t40, observed).
narrative_ontology:measurement(us_c_su_t50, us_constitution_text__living_constitutionalist_reading, suppression_requirement, 50, 0.28).
narrative_ontology:measurement_basis(us_c_su_t50, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_text__living_constitutionalist_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(us_constitution_text__living_constitutionalist_reading, 0.12).
narrative_ontology:affects_constraint(us_constitution_text__living_constitutionalist_reading, us_constitution_text__originalist_reading).
narrative_ontology:affects_constraint(us_constitution_text__living_constitutionalist_reading, us_constitution_text__positivist_reading).

% DUAL FORMULATION NOTE:
% The us_constitution_text kernel decomposes into three constraint stories: living_constitutionalist_reading (this file), originalist_reading, and positivist_reading. Each instantiates a different reading with different beneficiary/victim structures and different extractiveness profiles. They are linked because each reading's adoption would change the structure of the others. Living constitutionalism influences originalism by making originalist readings harder to institutionalize; originalism forecloses living constitutionalism within a single judicial framework (meaning cannot simultaneously be both fixed and adaptive). Positivism coexists with both: it addresses legitimacy independently of meaning and could be combined with either reading of substantive meaning.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
