% ============================================================================
% CONSTRAINT STORY: constitutional_text__popular_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_text__popular_sovereignty_reading, []).

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
 *   constraint_id: constitutional_text__popular_sovereignty_reading
 *   human_readable: Constitutional Text as Constituent-Power Reservoir (Popular Sovereignty Reading)
 *   domain: constitutional_theory/political_philosophy/comparative_law
 *
 * SUMMARY:
 *   This story instantiates the popular-sovereignty reading of the
 *   constitutional-text kernel: constitutional authority is understood to
 *   originate in and remain retained by the constituent power of the demos,
 *   such that neither courts nor legislatures hold final interpretive
 *   authority — the people retain recourse through amendment, convention, or
 *   (in the limiting case) revolution. This is one of three structurally
 *   distinct constraints sharing the same textual object; the
 *   judicial-supremacy reading and the legislative-sovereignty reading are
 *   separate stories with their own epsilon values, beneficiary/victim
 *   structures, and classifications. Under this reading, the coordination
 *   function (a legitimate outlet for correcting institutional failure) is
 *   real, but it is coupled to an extraction dynamic: organized mobilized
 *   movements and populist entrepreneurs gain standing and leverage from the
 *   doctrine's availability, while career judiciary, constitutional experts,
 *   and — most importantly — minority rights claimants who cannot match
 *   majoritarian mobilization capacity bear the resulting instability and
 *   exposure. That coupling is why this reading classifies as tangled_rope
 *   rather than a pure rope: genuine coordination function plus asymmetric
 *   extraction, both riding the same doctrinal structure, requiring active
 *   enforcement (recognition, convention procedures, amendment thresholds) to
 *   persist.
 *
 * KEY AGENTS:
 *   - mobilized_constituent_movements: primary beneficiary (organized/mobile) — gains standing and leverage from the doctrine's availability
 *   - career_judiciary: primary target (institutional/constrained) — bears delegitimization risk and reduced settlement finality
 *   - minority_rights_claimants: secondary target (powerless/trapped) — bears the sharpest cost, lacking mobilization capacity
 *   - comparative_constitutional_scholars: analytical observer — compares this reading's outcomes against judicial-supremacy and legislative-sovereignty siblings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_text__popular_sovereignty_reading, 0.58).
domain_priors:suppression_score(constitutional_text__popular_sovereignty_reading, 0.62).
domain_priors:theater_ratio(constitutional_text__popular_sovereignty_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_text__popular_sovereignty_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(constitutional_text__popular_sovereignty_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(constitutional_text__popular_sovereignty_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_text__popular_sovereignty_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(constitutional_text__popular_sovereignty_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_text__popular_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_text__popular_sovereignty_reading, "Constitutional Text as Constituent-Power Reservoir (Popular Sovereignty Reading)").
narrative_ontology:topic_domain(constitutional_text__popular_sovereignty_reading, "constitutional_theory/political_philosophy/comparative_law").

domain_priors:requires_active_enforcement(constitutional_text__popular_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_text__popular_sovereignty_reading, '300552b2-be65-43bd-9ff6-a013f36d6637').
narrative_ontology:cs_kernel_codification('300552b2-be65-43bd-9ff6-a013f36d6637', fixed_text).
narrative_ontology:cs_authority_grounding('300552b2-be65-43bd-9ff6-a013f36d6637', distributed).
narrative_ontology:cs_reading_relation('300552b2-be65-43bd-9ff6-a013f36d6637', constitutional_text__judicial_supremacy_reading, forecloses).
narrative_ontology:cs_reading_relation('300552b2-be65-43bd-9ff6-a013f36d6637', constitutional_text__legislative_sovereignty_reading, coexists_with).
narrative_ontology:cs_axiom('300552b2-be65-43bd-9ff6-a013f36d6637', foundational, constituent_power_never_alienated).
narrative_ontology:cs_axiom_status(constituent_power_never_alienated, holdable).
narrative_ontology:cs_axiom_grounding('300552b2-be65-43bd-9ff6-a013f36d6637', constituent_power_never_alienated, deontological).
narrative_ontology:cs_axiom('300552b2-be65-43bd-9ff6-a013f36d6637', secondary, institutional_finality_is_provisional).
narrative_ontology:cs_axiom_status(institutional_finality_is_provisional, holdable).
narrative_ontology:cs_axiom_grounding('300552b2-be65-43bd-9ff6-a013f36d6637', institutional_finality_is_provisional, conventional).
narrative_ontology:cs_reference_frame('300552b2-be65-43bd-9ff6-a013f36d6637', founding_era_constituent_assembly_authority).
narrative_ontology:cs_drift_state('300552b2-be65-43bd-9ff6-a013f36d6637', contemporary_judicial_review_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('300552b2-be65-43bd-9ff6-a013f36d6637', '').
narrative_ontology:cs_kernel_id(constitutional_text__popular_sovereignty_reading, constitutional_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_text__popular_sovereignty_reading, mobilized_constituent_movements).
narrative_ontology:constraint_beneficiary(constitutional_text__popular_sovereignty_reading, amendment_convention_organizers).
narrative_ontology:constraint_beneficiary(constitutional_text__popular_sovereignty_reading, populist_political_entrepreneurs).
narrative_ontology:constraint_victim(constitutional_text__popular_sovereignty_reading, career_judiciary).
narrative_ontology:constraint_victim(constitutional_text__popular_sovereignty_reading, constitutional_experts).
narrative_ontology:constraint_victim(constitutional_text__popular_sovereignty_reading, minority_rights_claimants).
narrative_ontology:constraint_victim(constitutional_text__popular_sovereignty_reading, institutional_continuity_administrators).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(constitutional_text__popular_sovereignty_reading, ordinary_voters).
narrative_ontology:constraint_vindicates(constitutional_text__popular_sovereignty_reading, constituent_power_doctrine).
narrative_ontology:constraint_vindicates(constitutional_text__popular_sovereignty_reading, popular_sovereignty_supremacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Organize referenda, amendment drives, and constitutional conventions to assert that the demos, not courts or legislatures, holds final interpretive authority. When mobilization succeeds, it can overturn settled judicial doctrine or legislative arrangements entirely by appeal to the people acting outside ordinary institutional channels. Their standing to act depends on demonstrating genuine popular mandate, which they can build episodically through campaigns, petitions, and mass mobilization.
narrative_ontology:constraint_stakeholder(constitutional_text__popular_sovereignty_reading, mobilized_constituent_movements, beneficiary,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(constitutional_text__popular_sovereignty_reading, mobilized_constituent_movements, agenda_setter).

% Operate the formal machinery (constitutional conventions, amendment petitions, referendum campaigns) through which constituent power is exercised. They gain standing, funding, and political capital whenever the popular-sovereignty reading is invoked to bypass or override judicial and legislative settlement. Their function is meaningful only when a live invocation channel exists.
narrative_ontology:constraint_stakeholder(constitutional_text__popular_sovereignty_reading, amendment_convention_organizers, beneficiary,
    organized, generational, mobile, national).

% Build careers and coalitions by appealing directly to constituent power against courts and legislatures, framing unfavorable judicial rulings or legislative deadlock as illegitimate usurpations of a sovereignty that properly belongs to the people. They benefit from the reading's availability whether or not any given mobilization succeeds, because the doctrine itself legitimizes their appeal.
narrative_ontology:constraint_stakeholder(constitutional_text__popular_sovereignty_reading, populist_political_entrepreneurs, beneficiary,
    powerful, biographical, mobile, national).

% Issue rulings under the constant background threat that any decision can be delegitimized as a mere institutional artifact subordinate to an ill-defined popular will. Cannot exit the jurisdiction or resign from being subject to constituent-power challenges; their doctrinal settlements carry less finality than in judicial-supremacy systems, and their institutional authority is structurally contingent on continued deference from mobilized constituencies.
narrative_ontology:constraint_stakeholder(constitutional_text__popular_sovereignty_reading, career_judiciary, payer,
    institutional, biographical, constrained, national).

% Provide doctrinal analysis and precedent-based reasoning that the popular-sovereignty reading treats as provisional at best — subject to override by extra-institutional mobilization regardless of technical merit. Their expertise is devalued whenever constituent-power claims succeed in overturning carefully reasoned settlements; they cannot exit the profession's dependence on the constitutional order remaining coherent.
narrative_ontology:constraint_stakeholder(constitutional_text__popular_sovereignty_reading, constitutional_experts, payer,
    moderate, biographical, constrained, national).

% Depend on stable judicial protection of rights that majoritarian mobilization can, under this reading, override by appeal to constituent power. When popular sovereignty is invoked against a judicial rights ruling, minority claimants have no institutional backstop above the mobilized majority; they cannot organize equivalent mass mobilization from a position of numerical and structural disadvantage, and cannot exit the polity's jurisdiction easily.
narrative_ontology:constraint_stakeholder(constitutional_text__popular_sovereignty_reading, minority_rights_claimants, payer,
    powerless, biographical, trapped, national).

% Maintain the ordinary functioning of courts, legislatures, and administrative agencies, whose stability is threatened whenever the popular-sovereignty reading successfully asserts that any settled arrangement is provisional and revisable by constituent mobilization. They bear the diffuse cost of institutional volatility, re-litigation, and legitimacy crises each time the reading is invoked.
narrative_ontology:constraint_stakeholder(constitutional_text__popular_sovereignty_reading, institutional_continuity_administrators, payer,
    institutional, generational, constrained, national).

% In principle the source of the constituent power this reading vests with supreme authority, but in practice participate through the organized movements and entrepreneurs who translate diffuse popular sentiment into actionable mobilization. Individually they have almost no capacity to invoke the doctrine; the reading's benefit to them is aggregate and mediated, not direct.
narrative_ontology:constraint_stakeholder(constitutional_text__popular_sovereignty_reading, ordinary_voters, beneficiary,
    powerless, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(constitutional_text__popular_sovereignty_reading, ordinary_voters, excluded).

% Study how popular-sovereignty, judicial-supremacy, and legislative-sovereignty readings of constitutional authority operate across jurisdictions, comparing outcomes for institutional stability, minority protection, and democratic responsiveness without themselves being subject to any single constitutional order's enforcement.
narrative_ontology:constraint_stakeholder(constitutional_text__popular_sovereignty_reading, comparative_constitutional_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(constitutional_text__popular_sovereignty_reading, diffuse).
narrative_ontology:fixing_cost_class(constitutional_text__popular_sovereignty_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a channel through which fundamental disagreement about constitutional meaning can be resolved by direct appeal to the polity itself when courts and legislatures reach impasse or lose legitimacy — amendment, convention, and (in extremis) revolutionary reconstitution give the demos a formal or quasi-formal outlet rather than leaving change to pure force.
% TRANSFER_FUNCTION: Moves final interpretive authority away from career judiciary and ordinary legislative majorities toward whichever actors can successfully claim to speak for constituent power — organized movements, convention organizers, and populist entrepreneurs — while shifting the cost of resulting instability onto institutional administrators, legal experts, and structurally weaker rights-claimants who cannot mobilize equivalent mass support.
% ABSENT_VOICES: Minority rights claimants who would be protected by durable judicial settlement have little voice in constituent-power mobilization, which structurally favors numerically larger and better-organized constituencies; they are formally 'the people' too but are absent from the mobilizations that actually exercise this reading's authority.
% DISAPPEARANCE_RATIONALE: If the popular-sovereignty reading vanished overnight — if constituent power were no longer recognized as superordinate to courts and legislatures — amendment campaigns and convention movements would lose their claimed constitutional warrant, judicial and legislative settlements would become effectively final, and populist entrepreneurs would lose their most powerful framing device for challenging institutional rulings. Institutional stability would rise; extra-institutional correction of institutional overreach would become harder.
% FOUNDING_PROBLEM: Constitutions need a source of legitimacy that can survive the corruption or capture of any single institution — courts can be packed, legislatures can be gerrymandered or captured by factions, and a purely institutional account of constitutional authority has no answer when the institutions themselves fail. The popular-sovereignty reading was built to preserve a legitimate path to fundamental change when ordinary institutional channels are exhausted, corrupted, or unresponsive.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional historians and comparative scholars outside any single mobilized movement attest that the founding problem (institutional capture with no legitimate corrective) remains real in some systems and largely dormant in others; career judiciary and institutional continuity administrators — parties who bear this reading's costs — attest that in stable democracies the problem is functionally dead and the doctrine now mainly supplies rhetorical cover for majoritarian overreach against minority protections, rather than solving a live legitimacy gap.
narrative_ontology:disappearance_verdict(constitutional_text__popular_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_text__popular_sovereignty_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_text__popular_sovereignty_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(constitutional_text__popular_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_text__popular_sovereignty_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_text__popular_sovereignty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(constitutional_text__popular_sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(constitutional_text__popular_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) reflects that the doctrine's operation systematically transfers final-say leverage toward whoever can mobilize, which is not evenly distributed across the polity — minority claimants and technocratic experts structurally cannot compete with organized majoritarian mobilization. Suppression (0.62) is higher than extraction because sustaining the doctrine requires active institutional tolerance of its own potential override (courts must accept that their rulings are provisional, legislatures must accept that statutes are revisable by extra-institutional means) — this is a real coercive/structural cost independent of how much is actually extracted in any given case. Theater ratio (0.42) is moderate: many invocations of 'the people's will' are genuine mobilizations, but a rising share over time is rhetorical invocation by entrepreneurs who never intend to complete a convention or amendment process, using the doctrine's legitimacy without its full procedural machinery. Accessibility collapse is comparatively low (0.4) because, unlike a mountain, meaningful alternatives (judicial settlement, legislative supremacy) remain fully available and contested — this is precisely why it is not a mountain. Resistance is high (0.72) because institutional actors actively resist the doctrine's application whenever it threatens settled rulings.
 *
 * PERSPECTIVAL GAP:
 *   From the seat of mobilized constituent movements, this reading is democracy correcting itself — genuine coordination against institutional drift. From the seat of minority rights claimants, the identical structure is exposure: a doctrine that can, at any point, subordinate their judicially-secured protections to whichever majority can organize loudest. The engine computes these as different seat-level classifications from the same structural data; this story does not adjudicate which seat is correct.
 *
 * DIRECTIONALITY LOGIC:
 *   Mobilized constituent movements, convention organizers, and populist entrepreneurs sit near the beneficiary end: the doctrine's mere availability increases their leverage regardless of any single outcome, and their exit options (mobile — they can redirect mobilization to new targets) put them structurally close to d=0. Career judiciary and institutional continuity administrators sit near the target end: their exit options are constrained (they cannot resign from being subject to constituent-power claims without abandoning their institutional role entirely), which pushes their derived d upward. Minority rights claimants are the sharpest case: powerless power atom plus trapped exit options derive the highest d in the story — they cannot mobilize equivalent constituent power and cannot exit the jurisdiction, so the doctrine's override capacity falls almost entirely on them without recourse.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled_rope (not snare) preserves the fact that a genuine coordination function exists — a legitimate corrective channel for institutional capture is not fabricated cover; historically-attested constitutional crises (courts packed by faction, legislatures captured by narrow interests) are real problems this reading answers. Calling it a pure snare would deny that founding problem's historical reality. But calling it a pure rope would deny the asymmetric cost borne by minority claimants and experts, and would ignore that its persistence now depends on active enforcement (recognition of convention procedures, amendment-threshold rules) rather than voluntary, net-beneficial participation by all parties. The founding_problem_status is authored as contested precisely because whether the corrective channel is still needed, or has degraded into rhetorical cover for majoritarian override, differs sharply between corroborating sources.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    constituent_power_genuine_vs_instrumentalized,
    'Is invocation of ''the people''s constituent power'' in a given episode a genuine expression of diffuse popular will, or is it manufactured/instrumentalized by organized political entrepreneurs claiming to speak for a constituency that was never actually mobilized in the numbers claimed?',
    'Post-hoc analysis of actual participation rates, petition/referendum turnout, and convention delegate representativeness relative to the total polity, compared against the rhetorical claims made at the time of invocation.',
    'If systematically instrumentalized, effective extraction is higher than the base metric suggests, because the doctrine''s leverage is being captured by organized minorities claiming majoritarian warrant. If genuinely diffuse and representative, the coordination function is closer to what the doctrine claims for itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constituent_power_genuine_vs_instrumentalized, empirical, 'Whether invoked constituent power reflects genuine mass mobilization or entrepreneurial capture of the doctrine''s rhetoric.').

omega_variable(
    kernel_reading_choice_ambiguity,
    'The constitutional_text kernel supports three coherent readings (judicial_supremacy, legislative_sovereignty, popular_sovereignty) with no textual feature of most constitutions definitively selecting one over the others — the choice is itself a contested interpretive act. Which reading a given legal system ''actually'' operates under is frequently underdetermined by text alone and settled instead by political practice, judicial self-restraint norms, or historical convention.',
    'Comparative constitutional analysis of how a given jurisdiction''s institutions have actually resolved conflicts between courts, legislatures, and popular mobilization over time — which reading''s predictions match observed institutional behavior during genuine constitutional crises.',
    'If a jurisdiction''s practice consistently defers to judicial finality even during crises, this reading''s claimed authority structure may be aspirational rather than operative there, which would lower its effective ε in that context (fewer genuine invocations succeed) while leaving the doctrine''s rhetorical availability unchanged.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_choice_ambiguity, conceptual, 'Whether the popular-sovereignty reading is the operative account of a given constitutional order or one contested framing among several with no textual tie-breaker.').

omega_variable(
    revolution_as_limiting_case_ambiguity,
    'The reading includes revolution as an ultimate recourse of constituent power. Is revolution properly part of the SAME constitutional constraint (an extreme point on the amendment/convention continuum) or does invoking revolution exit the constitutional order entirely, making it a structurally different phenomenon (extra-legal, extra-constitutional) that should not be scored within this constraint''s ε at all?',
    'Doctrinal analysis of whether any constitutional text or tradition treats revolution as an internal safety valve (as some natural-law and social-contract traditions do) versus treating it as definitionally outside the constitutional order (as most positivist accounts do).',
    'If revolution is treated as outside the constraint, ε should be somewhat lower (the doctrine''s practical extraction runs through amendment and convention mechanisms only); if treated as within it, the doctrine''s suppression score should be higher, since the ultimate enforcement mechanism backing this reading is the credible threat of extra-legal rupture.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(revolution_as_limiting_case_ambiguity, conceptual, 'Whether revolution belongs inside this constraint''s scope or marks its outer boundary.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_text__popular_sovereignty_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t0, constitutional_text__popular_sovereignty_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(cons_tr_t10, constitutional_text__popular_sovereignty_reading, theater_ratio, 10, 0.29).
narrative_ontology:measurement(cons_tr_t20, constitutional_text__popular_sovereignty_reading, theater_ratio, 20, 0.33).
narrative_ontology:measurement(cons_tr_t30, constitutional_text__popular_sovereignty_reading, theater_ratio, 30, 0.36).
narrative_ontology:measurement(cons_tr_t40, constitutional_text__popular_sovereignty_reading, theater_ratio, 40, 0.38).
narrative_ontology:measurement(cons_tr_t50, constitutional_text__popular_sovereignty_reading, theater_ratio, 50, 0.4).
narrative_ontology:measurement(cons_tr_t60, constitutional_text__popular_sovereignty_reading, theater_ratio, 60, 0.42).

% Extraction over time
narrative_ontology:measurement(cons_be_t0, constitutional_text__popular_sovereignty_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(cons_be_t10, constitutional_text__popular_sovereignty_reading, base_extractiveness, 10, 0.44).
narrative_ontology:measurement(cons_be_t20, constitutional_text__popular_sovereignty_reading, base_extractiveness, 20, 0.49).
narrative_ontology:measurement(cons_be_t30, constitutional_text__popular_sovereignty_reading, base_extractiveness, 30, 0.52).
narrative_ontology:measurement(cons_be_t40, constitutional_text__popular_sovereignty_reading, base_extractiveness, 40, 0.55).
narrative_ontology:measurement(cons_be_t50, constitutional_text__popular_sovereignty_reading, base_extractiveness, 50, 0.57).
narrative_ontology:measurement(cons_be_t60, constitutional_text__popular_sovereignty_reading, base_extractiveness, 60, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t0, constitutional_text__popular_sovereignty_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(cons_su_t10, constitutional_text__popular_sovereignty_reading, suppression_requirement, 10, 0.52).
narrative_ontology:measurement(cons_su_t20, constitutional_text__popular_sovereignty_reading, suppression_requirement, 20, 0.55).
narrative_ontology:measurement(cons_su_t30, constitutional_text__popular_sovereignty_reading, suppression_requirement, 30, 0.58).
narrative_ontology:measurement(cons_su_t40, constitutional_text__popular_sovereignty_reading, suppression_requirement, 40, 0.6).
narrative_ontology:measurement(cons_su_t50, constitutional_text__popular_sovereignty_reading, suppression_requirement, 50, 0.61).
narrative_ontology:measurement(cons_su_t60, constitutional_text__popular_sovereignty_reading, suppression_requirement, 60, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_text__popular_sovereignty_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(constitutional_text__popular_sovereignty_reading, judicial_supremacy_reading).
narrative_ontology:affects_constraint(constitutional_text__popular_sovereignty_reading, legislative_sovereignty_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three siblings decomposing the natural-language concept 'where does final constitutional interpretive authority reside' (the constitutional_text kernel). judicial_supremacy_reading holds courts final; legislative_sovereignty_reading holds parliament final; this story (popular_sovereignty_reading) holds neither final, vesting ultimate authority in extra-institutional constituent power. Each has a distinct beneficiary/victim structure and epsilon: judicial_supremacy is expected to score lower extraction with experts/courts as beneficiaries; legislative_sovereignty is expected to score moderate extraction with legislative majorities as beneficiaries and courts/minorities as more exposed; this popular_sovereignty reading scores moderate-to-substantial extraction because its coordination benefit (a corrective valve against institutional capture) is coupled with the asymmetric cost borne by minority claimants and institutional experts who cannot match majoritarian mobilization capacity. All three are linked via affects_constraints because a shift in one jurisdiction's practical adherence to one reading directly changes the legitimacy conditions and resource availability for invoking the other two.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
