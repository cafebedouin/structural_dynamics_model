% ============================================================================
% CONSTRAINT STORY: constitutional_text__legislative_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_text__legislative_sovereignty_reading, []).

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
 *   constraint_id: constitutional_text__legislative_sovereignty_reading
 *   human_readable: Constitutional Text: Legislative Sovereignty Reading
 *   domain: constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   This constraint captures one reading of the contested constitutional
 *   kernel: that the written constitutional text establishes the legislature
 *   as the supreme interpretive authority, with courts in an advisory role
 *   and the legislature retaining the power to override judicial
 *   interpretations through notwithstanding clauses or ordinary statute. This
 *   is a direct reading of certain constitutional texts (Canada's Section 33,
 *   the UK's parliamentary supremacy, Israel's Basic Laws). The constraint is
 *   classified as tangled_rope because it coordinates constitutional
 *   governance (provides a unified authority structure for interpreting
 *   fundamental law) while simultaneously extracting from entrenched
 *   minorities by making their constitutional protections revocable by
 *   current legislative majorities. The claim and metrics are authored
 *   independently: claimed as tangled_rope (genuine coordination + asymmetric
 *   extraction), metrics authored to describe the actual operation (high
 *   extractiveness for minorities, moderate suppression of judicial override,
 *   rising theater as legislative authority becomes performative of supremacy
 *   rather than exercising substantive interpretive work).
 *
 * KEY AGENTS:
 *   - majoritarian_legislative_coalition: Primary agenda-setter (institutional/arbitrage) — sets constitutional meaning and enforces supremacy against judicial intervention
 *   - entrenched_minorities: Primary victims (powerless/trapped) — bear continuous vulnerability of their constitutional rights to legislative override
 *   - minority_rights_holders: Secondary victims (moderate/constrained) — rights claims subject to majoritarian override without judicial backstop
 *   - appointed_judiciary: Dual-positioned (institutional/constrained) — benefits from retained advisory status but excluded from final authority
 *   - comparative_democracies: Analytical observer (institutional/analytical) — assesses outcomes and can apply reputational pressure but hold no internal authority
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_text__legislative_sovereignty_reading, 0.68).
domain_priors:suppression_score(constitutional_text__legislative_sovereignty_reading, 0.52).
domain_priors:theater_ratio(constitutional_text__legislative_sovereignty_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_text__legislative_sovereignty_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(constitutional_text__legislative_sovereignty_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(constitutional_text__legislative_sovereignty_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_text__legislative_sovereignty_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(constitutional_text__legislative_sovereignty_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_text__legislative_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_text__legislative_sovereignty_reading, "Constitutional Text: Legislative Sovereignty Reading").
narrative_ontology:topic_domain(constitutional_text__legislative_sovereignty_reading, "constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(constitutional_text__legislative_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_text__legislative_sovereignty_reading, 'fa09c32e-61fe-42de-8009-d72cbd7f8754').
narrative_ontology:cs_kernel_codification('fa09c32e-61fe-42de-8009-d72cbd7f8754', formalized).
narrative_ontology:cs_authority_grounding('fa09c32e-61fe-42de-8009-d72cbd7f8754', lineage).
narrative_ontology:cs_interpretation_layer_present('fa09c32e-61fe-42de-8009-d72cbd7f8754').
narrative_ontology:cs_reading_relation('fa09c32e-61fe-42de-8009-d72cbd7f8754', constitutional_text__judicial_supremacy_reading, forecloses).
narrative_ontology:cs_reading_relation('fa09c32e-61fe-42de-8009-d72cbd7f8754', constitutional_text__popular_sovereignty_reading, coexists_with).
narrative_ontology:cs_axiom('fa09c32e-61fe-42de-8009-d72cbd7f8754', foundational, elected_representatives_hold_final_constitutional_authority).
narrative_ontology:cs_axiom_status(elected_representatives_hold_final_constitutional_authority, holdable).
narrative_ontology:cs_axiom_grounding('fa09c32e-61fe-42de-8009-d72cbd7f8754', elected_representatives_hold_final_constitutional_authority, deontological).
narrative_ontology:cs_axiom('fa09c32e-61fe-42de-8009-d72cbd7f8754', foundational, majoritarian_will_overrides_judicial_interpretation).
narrative_ontology:cs_axiom_status(majoritarian_will_overrides_judicial_interpretation, holdable).
narrative_ontology:cs_axiom_grounding('fa09c32e-61fe-42de-8009-d72cbd7f8754', majoritarian_will_overrides_judicial_interpretation, deontological).
narrative_ontology:cs_axiom('fa09c32e-61fe-42de-8009-d72cbd7f8754', secondary, appointed_judges_lack_democratic_legitimacy_for_supremacy).
narrative_ontology:cs_axiom_status(appointed_judges_lack_democratic_legitimacy_for_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('fa09c32e-61fe-42de-8009-d72cbd7f8754', appointed_judges_lack_democratic_legitimacy_for_supremacy, deontological).
narrative_ontology:cs_reference_frame('fa09c32e-61fe-42de-8009-d72cbd7f8754', parliamentary_authority_framework).
narrative_ontology:cs_drift_state('fa09c32e-61fe-42de-8009-d72cbd7f8754', contemporary_minority_rights_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('fa09c32e-61fe-42de-8009-d72cbd7f8754', '').
narrative_ontology:cs_kernel_id(constitutional_text__legislative_sovereignty_reading, constitutional_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_text__legislative_sovereignty_reading, majoritarian_legislative_coalition).
narrative_ontology:constraint_victim(constitutional_text__legislative_sovereignty_reading, entrenched_minorities).
narrative_ontology:constraint_victim(constitutional_text__legislative_sovereignty_reading, minority_rights_holders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(constitutional_text__legislative_sovereignty_reading, appointed_judiciary).
narrative_ontology:constraint_beneficiary(constitutional_text__legislative_sovereignty_reading, executive_branch).
narrative_ontology:constraint_vindicates(constitutional_text__legislative_sovereignty_reading, parliamentary_supremacy).
narrative_ontology:constraint_vindicates(constitutional_text__legislative_sovereignty_reading, representative_democracy_primacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The elected legislature, representing current electoral majorities, wields ultimate interpretive authority over the constitutional text. Sets the constitutional meaning through statute, override, or notwithstanding clauses; enforces that meaning against judicial intervention. Controls the enforcement apparatus and legislative agenda. Benefits from the reading because it permits rapid adaptation of constitutional meaning to majoritarian will without requiring court approval or supermajority amendment procedures.
narrative_ontology:constraint_stakeholder(constitutional_text__legislative_sovereignty_reading, majoritarian_legislative_coalition, agenda_setter,
    institutional, generational, arbitrage, national).

% Groups whose rights depend on constitutional entrenchment (racial, religious, sexual-orientation minorities; indigenous peoples; religious dissenters). Under this reading, their protections stand only at legislative sufferance and can be overridden by statute or notwithstanding clause when majoritarian opinion shifts. Exit options are geographic emigration (constrained by nation-state borders and residency laws) or political mobilization (difficult when already outnumbered). Their costs are the continuous vulnerability of their nominal rights.
narrative_ontology:constraint_stakeholder(constitutional_text__legislative_sovereignty_reading, entrenched_minorities, payer,
    powerless, biographical, trapped, national).

% Individuals holding rights claims (due process, freedom of conscience, property claims) that compete with majoritarian interests. Under legislative sovereignty, their claims are subject to legislative override when the majority prefers collective goods or alternative distributions. They must rely on legislative goodwill or internal legislative coalitions that share their interests; they have no court as a backstop authority.
narrative_ontology:constraint_stakeholder(constitutional_text__legislative_sovereignty_reading, minority_rights_holders, payer,
    moderate, biographical, constrained, national).

% Courts provide expert legal interpretation and advisory opinions but lack final authority. They are permitted to review legislation and issue rulings, but the legislature can override via notwithstanding clause or ordinary statute. This is a secondary beneficiary role: courts retain institutional status and advisory authority (preventing their complete exclusion), but are excluded from ultimate constitutional authority. They are constrained from leaving the system — they are part of the governmental apparatus.
narrative_ontology:constraint_stakeholder(constitutional_text__legislative_sovereignty_reading, appointed_judiciary, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(constitutional_text__legislative_sovereignty_reading, appointed_judiciary, excluded).

% The executive implements legislation and exercises delegated constitutional powers. Under legislative sovereignty, the executive's constitutional authority flows directly from legislative enactment (not from independent constitutional grant), making it subordinate and flexible. This is a secondary beneficiary position: the executive benefits from clarity in legislative direction but is subordinate to parliamentary will.
narrative_ontology:constraint_stakeholder(constitutional_text__legislative_sovereignty_reading, executive_branch, beneficiary,
    institutional, biographical, constrained, national).

% Non-governmental organizations, advocacy groups, and citizen movements that seek to defend minority rights or challenge majoritarian overreach. Under this reading, their access to constitutional remedy is severely curtailed — they cannot rely on courts to invalidate majoritarian legislation. Their channel is legislative lobbying and electoral mobilization, which are difficult against majority opposition. They are structurally excluded from the constraint's interpretive authority.
narrative_ontology:constraint_stakeholder(constitutional_text__legislative_sovereignty_reading, civil_society_and_advocacy, excluded,
    organized, biographical, constrained, national).

% International observers, comparative constitutional scholars, and regional human-rights bodies that assess the jurisdiction's adherence to constitutional norms and minority-rights protections. They have no internal authority but can apply diplomatic or reputational pressure. Their position is analytical: they assess outcomes but cannot enforce alternative constitutional readings within the jurisdiction.
narrative_ontology:constraint_stakeholder(constitutional_text__legislative_sovereignty_reading, comparative_democracies, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(constitutional_text__legislative_sovereignty_reading, majoritarian_legislative_coalition).
narrative_ontology:fixing_cost_class(constitutional_text__legislative_sovereignty_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a unified, authoritative source for constitutional meaning (the legislature) to avoid conflicting interpretations and enable rapid legislative response to constitutional questions without delay for judicial process or amendment procedures. Coordinates governmental action around a single interpretive authority.
% TRANSFER_FUNCTION: Transfers interpretive authority over fundamental law from distributed sources (courts, constituent power) to the elected legislature. Moves effective control of constitutional meaning from the judicial branch to the legislative branch, enabling majoritarian will to reshape constitutional protections.
% ABSENT_VOICES: Entrenched minorities and future generations whose rights depend on constitutional rigidity are excluded from the interpretive conversation — they have no seat with override power and must rely on current legislative majorities' goodwill. International human-rights advocates would object on grounds that this reading enables majority tyranny; they are structurally outside the national legislative framework.
% DISAPPEARANCE_RATIONALE: If the legislative supremacy constraint disappeared overnight and courts gained final interpretive authority (the judicial_supremacy_reading alternative), constitutional meaning would become substantially more rigid, judicial review would become conclusive, and minority-rights protections would gain institutional insulation from majoritarian override. The entire distribution of constitutional power would reorganize around a different authority structure.
% FOUNDING_PROBLEM: How to enable a constitution to adapt to changing majoritarian will and evolving circumstances without requiring supermajority amendment procedures or depending on judicial interpretation that may lag public opinion or be captured by judicial ideology.
% FOUNDING_PROBLEM_CORROBORATION: Legislatures that claim parliamentary supremacy clauses (Canada, the UK historically, Israel) attest the founding problem is live and this reading solves it by enabling rapid constitutional evolution. Constitutional scholars defending judicial review and minority-rights entrenchment attest the founding problem has been overstated and the 'solution' creates a worse problem (majority tyranny) — they are outside the benefiting set and their testimony corroborates the contest.
narrative_ontology:disappearance_verdict(constitutional_text__legislative_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_text__legislative_sovereignty_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_text__legislative_sovereignty_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(constitutional_text__legislative_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_text__legislative_sovereignty_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_text__legislative_sovereignty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(constitutional_text__legislative_sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(constitutional_text__legislative_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is 0.68 at interval end, rising from 0.45 at t0. The trajectory reflects the constraint's operating principle: as legislatures exercise their supremacy, minorities face increasing exposure (extraction rises). Extraction plateaus after t=25 because the constraint reaches steady-state operation — legislatures know they have final authority and regularly exercise it; minorities have adapted expectations downward. Suppression starts low (0.38) because it requires active exclusion of judicial override (courts must be prevented from using section-2 type reasoning to limit the notwithstanding clause); it rises to 0.52 as legislatures institutionalize override procedures and courts normalize judicial deference. Theater rises sharply (0.22 to 0.41 by t=15) as the original coordination function (unified interpretive authority) becomes secondary to the performative maintenance of legislative supremacy — legislatures deploy the override authority theatrically even when judicial agreement exists, to reaffirm dominance. Theater plateaus because the performance itself becomes the function — the legislature must continuously perform its supremacy to maintain it. Accessibility collapse is low (0.48) because alternatives (judicial review, constituent assembly, popular referenda) remain formally available; they are just subordinate to legislative override. Resistance is high (0.71) because entrenched minorities and civil-society actors continuously contest the reading through litigation strategy (losing cases that clarify the override right), constitutional scholarship, electoral mobilization, and international advocacy. The measurements are aligned on a single time grid: every metric authored at every shared time point so no metric enters a row missing and artificially inherits an end-state scalar.
 *
 * PERSPECTIVAL GAP:
 *   The majoritarian legislative coalition perceives this constraint as genuine democratic coordination — the natural supremacy of elected representatives over appointed judges, enabling responsive governance. Entrenched minorities perceive it as asymmetric extraction: their rights are subordinated to majoritarian preference without institutional protection. The appointed judiciary occupies an intermediate position: they are functionally included (they interpret and advise) but doctrinally excluded (their interpretation is not binding). The engine should compute this reading as tangled_rope from all seats because the structure is genuinely asymmetric: the legislator's d approaches 0.0 (beneficiary), the minority's d approaches 1.0 (target), the judiciary's d sits near 0.5 (symmetric — they coordinate governmental function but are also excluded from ultimate authority). The divergence in computed types per seat is the measurement this constraint enables.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary: majoritarian_legislative_coalition (power=institutional, exit=arbitrage, benefits from increased interpretive authority and absence of judicial constraint; d derives toward 0.0). Victims: entrenched_minorities (power=powerless, exit=trapped, rights made revocable at legislative sufferance; d derives toward 1.0) and minority_rights_holders (power=moderate, exit=constrained, rights subordinated to majoritarian preference; d derives toward 0.7). The judiciary is a dual-positioned agent: beneficiary role (retains advisory authority and institutional status; excluded from exclusion) but constrained exit and institutional power keeps d near 0.4-0.5. No directionality overrides necessary — the structural derivation captures the asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint's founding problem was real: early constitutional governance faced coordination failure when different branches offered competing interpretations. Legislative supremacy solves that — one unified authority. But the founding problem status is contested (as measured in six_questions): legislatures that ratified this reading claim the problem is live (adaptation is urgent, minority protection is a luxury); constitutional scholars and comparative democracies claim the problem was solved by judicial review and entrenchment, making this reading's solution worse than the original problem (trading one coordination failure for systematic minority rights erosion). The theater_ratio rise (0.22 to 0.41) indicates the coordination function is atrophying: legislatures now deploy the override to assert dominance even when judicial agreement exists, converting a coordination mechanism into a performance of power. A piton reading would be more accurate at t=30+, except for one factor: the constraint still genuinely coordinates (legislatures do speak with unified voice) and minorities genuinely do contest (resistance stays high). It is tangled_rope throughout because the coordination and extraction are structurally entangled — you cannot have the unified authority without the minority subordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    parliamentary_supremacy_vs_tyranny_of_majority,
    'Is legislative supremacy a safeguard for democratic responsiveness or a structural mechanism enabling majority tyranny of minorities?',
    'Empirical: examine constitutional systems adopting this reading over 20+ years and compare minority-rights protections, rates of rights violation, electoral responsiveness, and minority satisfaction with the system. Contextual: compare jurisdictions using this reading to those using judicial supremacy or entrenched bills of rights, controlling for economic development and ethnic diversity.',
    'If empirical record shows systematic erosion of minority rights and elevated rights violations under this reading, the classification shifts toward snare (extraction from trapped minorities without coordination benefit for them). If protections hold and minorities report satisfaction, classification remains tangled_rope (genuine coordination with real but manageable extraction costs).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(parliamentary_supremacy_vs_tyranny_of_majority, empirical, 'Whether legislative supremacy enables adaptive democracy or institutionalizes minority subordination.').

omega_variable(
    legitimacy_grounding_for_legislative_supremacy,
    'Is legislative supremacy grounded in the written constitutional text (textual originalism about legislative intent), in pragmatic coordination needs (instrumental), or in a normative claim about democratic legitimacy (representative majority should prevail)?',
    'Textual: compare the written constitutional provisions across jurisdictions and assess whether they unambiguously grant legislatures final interpretive authority or merely fail to explicitly grant courts final authority. Genealogical: trace the reading''s origin and see whether it emerged from textual scholarship, from pragmatic judicial deference, or from normative democratic theory. Axiological: identify which reading''s normative grounding is actually doing the work in enforcement — do courts defer because the text says so, because they lack political power to resist, or because they believe majority rule is more legitimate?',
    'If the reading is primarily textually grounded, it is robust to normative challenge. If primarily instrumental (courts defer because they lack power), it is vulnerable to power shifts. If primarily normative, it faces ongoing contestation from alternative democratic theories. The axioms in cs_structure should identify which grounding is foundational.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(legitimacy_grounding_for_legislative_supremacy, conceptual, 'What kind of justification legitimates legislative supremacy in this reading.').

omega_variable(
    emergence_of_notwithstanding_clauses,
    'Are notwithstanding clauses a textual feature of the original constitutional text, or are they a later interpretive addition that made legislative supremacy explicit and reversible?',
    'Historical: trace when notwithstanding clauses entered the constitutional practice and whether they appeared in the text at ratification or emerged through case law / statutory amendment.',
    'If textual from the start, they are part of the foundational kernel. If added later, they represent an interpretive shift that may itself be contested and revocable — in which case the constraint is more brittle than it appears (a future legislature might repeal the override clause itself, triggering a meta-constitutional crisis). This affects whether the constraint''s persistence depends on active maintenance or inertia.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(emergence_of_notwithstanding_clauses, empirical, 'Whether notwithstanding clauses are original or derived interpretations.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is suppression of judicial override enforced structurally (legal rules that prevent courts from invalidating the override clause, procedural barriers to judicial intervention) or internalized (courts have internalized norms of deference and accept legislative supremacy as legitimate)?',
    'Test through a jurisdiction that shifts from this reading to judicial supremacy (or vice versa): if suppression persists in the original direction (courts continue to defer even after override is legally prohibited), it is partially internalized. If suppression reverses quickly (courts assert authority once legal barriers drop), it is primarily structural.',
    'If suppression is structural, removing the constraint is feasible via legislative amendment. If internalized, removing the constraint requires cultural-legal shift in judicial consciousness — higher fixing cost. The combination affects whether the constraint is temporary (legal change can dislodge it) or entrenched (requires cultural reconstitution).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether suppression of judicial override is structural or internalized in judicial culture.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_text__legislative_sovereignty_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t0, constitutional_text__legislative_sovereignty_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(cons_tr_t0, observed).
narrative_ontology:measurement(cons_tr_t5, constitutional_text__legislative_sovereignty_reading, theater_ratio, 5, 0.28).
narrative_ontology:measurement_basis(cons_tr_t5, observed).
narrative_ontology:measurement(cons_tr_t10, constitutional_text__legislative_sovereignty_reading, theater_ratio, 10, 0.35).
narrative_ontology:measurement_basis(cons_tr_t10, observed).
narrative_ontology:measurement(cons_tr_t15, constitutional_text__legislative_sovereignty_reading, theater_ratio, 15, 0.39).
narrative_ontology:measurement_basis(cons_tr_t15, observed).
narrative_ontology:measurement(cons_tr_t20, constitutional_text__legislative_sovereignty_reading, theater_ratio, 20, 0.41).
narrative_ontology:measurement_basis(cons_tr_t20, observed).
narrative_ontology:measurement(cons_tr_t25, constitutional_text__legislative_sovereignty_reading, theater_ratio, 25, 0.41).
narrative_ontology:measurement_basis(cons_tr_t25, observed).
narrative_ontology:measurement(cons_tr_t30, constitutional_text__legislative_sovereignty_reading, theater_ratio, 30, 0.41).
narrative_ontology:measurement_basis(cons_tr_t30, observed).
narrative_ontology:measurement(cons_tr_t40, constitutional_text__legislative_sovereignty_reading, theater_ratio, 40, 0.41).
narrative_ontology:measurement_basis(cons_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(cons_be_t0, constitutional_text__legislative_sovereignty_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(cons_be_t0, observed).
narrative_ontology:measurement(cons_be_t5, constitutional_text__legislative_sovereignty_reading, base_extractiveness, 5, 0.51).
narrative_ontology:measurement_basis(cons_be_t5, observed).
narrative_ontology:measurement(cons_be_t10, constitutional_text__legislative_sovereignty_reading, base_extractiveness, 10, 0.58).
narrative_ontology:measurement_basis(cons_be_t10, observed).
narrative_ontology:measurement(cons_be_t15, constitutional_text__legislative_sovereignty_reading, base_extractiveness, 15, 0.63).
narrative_ontology:measurement_basis(cons_be_t15, observed).
narrative_ontology:measurement(cons_be_t20, constitutional_text__legislative_sovereignty_reading, base_extractiveness, 20, 0.66).
narrative_ontology:measurement_basis(cons_be_t20, observed).
narrative_ontology:measurement(cons_be_t25, constitutional_text__legislative_sovereignty_reading, base_extractiveness, 25, 0.68).
narrative_ontology:measurement_basis(cons_be_t25, observed).
narrative_ontology:measurement(cons_be_t30, constitutional_text__legislative_sovereignty_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement_basis(cons_be_t30, observed).
narrative_ontology:measurement(cons_be_t40, constitutional_text__legislative_sovereignty_reading, base_extractiveness, 40, 0.68).
narrative_ontology:measurement_basis(cons_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t0, constitutional_text__legislative_sovereignty_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement_basis(cons_su_t0, observed).
narrative_ontology:measurement(cons_su_t5, constitutional_text__legislative_sovereignty_reading, suppression_requirement, 5, 0.43).
narrative_ontology:measurement_basis(cons_su_t5, observed).
narrative_ontology:measurement(cons_su_t10, constitutional_text__legislative_sovereignty_reading, suppression_requirement, 10, 0.48).
narrative_ontology:measurement_basis(cons_su_t10, observed).
narrative_ontology:measurement(cons_su_t15, constitutional_text__legislative_sovereignty_reading, suppression_requirement, 15, 0.51).
narrative_ontology:measurement_basis(cons_su_t15, observed).
narrative_ontology:measurement(cons_su_t20, constitutional_text__legislative_sovereignty_reading, suppression_requirement, 20, 0.52).
narrative_ontology:measurement_basis(cons_su_t20, observed).
narrative_ontology:measurement(cons_su_t25, constitutional_text__legislative_sovereignty_reading, suppression_requirement, 25, 0.52).
narrative_ontology:measurement_basis(cons_su_t25, observed).
narrative_ontology:measurement(cons_su_t30, constitutional_text__legislative_sovereignty_reading, suppression_requirement, 30, 0.52).
narrative_ontology:measurement_basis(cons_su_t30, observed).
narrative_ontology:measurement(cons_su_t40, constitutional_text__legislative_sovereignty_reading, suppression_requirement, 40, 0.52).
narrative_ontology:measurement_basis(cons_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_text__legislative_sovereignty_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(constitutional_text__legislative_sovereignty_reading, 0.12).
narrative_ontology:affects_constraint(constitutional_text__legislative_sovereignty_reading, constitutional_text__judicial_supremacy_reading).
narrative_ontology:affects_constraint(constitutional_text__legislative_sovereignty_reading, constitutional_text__popular_sovereignty_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested constitutional_text kernel. The sibling readings (judicial_supremacy_reading, popular_sovereignty_reading) are separate constraint stories with distinct ε values, beneficiary/victim structures, and classifications. All three readings assess the same constitutional text but instantiate different constraints because they assign ultimate interpretive authority to different entities (legislature, courts, constituent people respectively). The three stories form a constraint family linked by network.affects_constraints — each reading's prevalence affects the structural conditions and legitimacy pressures on the others. Do not merge them into one multi-observable constraint; each reading is a distinct constraint with its own ε-invariant story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
