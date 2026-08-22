% ============================================================================
% CONSTRAINT STORY: legal_personhood_boundary__restrictive_anthropocentric_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legal_personhood_boundary__restrictive_anthropocentric_reading, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: legal_personhood_boundary__restrictive_anthropocentric_reading
 *   human_readable: Legal Personhood: Restrictive Anthropocentric Reading (Born Humans with Cognitive Capacity)
 *   domain: legal_philosophy/constitutional_law/rights_theory
 *
 * SUMMARY:
 *   The restrictive anthropocentric reading of legal personhood defines
 *   rights-bearing status narrowly: born humans with measurable cognitive
 *   capacity. This reading serves as one enforcement axis in a contested
 *   kernel — the legal personhood boundary — where multiple readings coexist
 *   in different jurisdictions and doctrinal traditions. The reading
 *   maximizes reproductive autonomy by excluding fetuses from the
 *   rights-bearing set; it minimizes environmental personhood claims by
 *   excluding ecosystems and non-human animals; it forecloses AI personhood
 *   by enforcing the born-human requirement categorically. The constraint is
 *   CLAIMED as tangled_rope because it coordinates reproductive autonomy
 *   (solving the coordination problem of how to prevent infinite contestation
 *   of personhood) while simultaneously extracting from fetuses, non-human
 *   animals, and alternative readings (reducing their standing, concentrating
 *   decisional authority in human-centered institutions). The extraction
 *   increases over the interval as bioethics and environmental law develop
 *   stronger cases for alternative readings, intensifying the need for active
 *   suppression of boundary-expansion arguments.
 *
 * KEY AGENTS:
 *   - Pregnant persons: benefit from exclusion of fetal personhood; maximize reproductive autonomy
 *   - Reproductive autonomy coalitions: benefit from legal stability of the boundary; organize its defense
 *   - Cognitive capacity gatekeepers (courts, legislatures): agenda-setters who enforce the boundary and suppress alternatives
 *   - Fetuses: structurally excluded from rights-bearing status; no standing in law
 *   - Non-human animals with documented cognitive capacity: excluded by the born-human requirement despite functional parity with the reading's criterion
 *   - Potentiality-rights advocates: excluded parties with organizational capacity but marginalized legal standing
 *   - Environmental personhood advocates: excluded from the personhood conversation by anthropocentric boundaries
 *   - Bioethics philosophers: observers documenting the reading's empirical fragility and normative contestability
 *   - Comparative legal systems: witnesses to alternative readings in other jurisdictions
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legal_personhood_boundary__restrictive_anthropocentric_reading, 0.68).
domain_priors:suppression_score(legal_personhood_boundary__restrictive_anthropocentric_reading, 0.71).
domain_priors:theater_ratio(legal_personhood_boundary__restrictive_anthropocentric_reading, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legal_personhood_boundary__restrictive_anthropocentric_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(legal_personhood_boundary__restrictive_anthropocentric_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(legal_personhood_boundary__restrictive_anthropocentric_reading, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legal_personhood_boundary__restrictive_anthropocentric_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(legal_personhood_boundary__restrictive_anthropocentric_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legal_personhood_boundary__restrictive_anthropocentric_reading, tangled_rope).
narrative_ontology:human_readable(legal_personhood_boundary__restrictive_anthropocentric_reading, "Legal Personhood: Restrictive Anthropocentric Reading (Born Humans with Cognitive Capacity)").
narrative_ontology:topic_domain(legal_personhood_boundary__restrictive_anthropocentric_reading, "legal_philosophy/constitutional_law/rights_theory").

domain_priors:requires_active_enforcement(legal_personhood_boundary__restrictive_anthropocentric_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legal_personhood_boundary__restrictive_anthropocentric_reading, 'd7c476b6-1440-4cb5-a9d4-95e305499c9d').
narrative_ontology:cs_kernel_codification('d7c476b6-1440-4cb5-a9d4-95e305499c9d', distributed).
narrative_ontology:cs_authority_grounding('d7c476b6-1440-4cb5-a9d4-95e305499c9d', lineage).
narrative_ontology:cs_interpretation_layer_present('d7c476b6-1440-4cb5-a9d4-95e305499c9d').
narrative_ontology:cs_reading_relation('d7c476b6-1440-4cb5-a9d4-95e305499c9d', legal_personhood_boundary__developmental_potentiality_reading, coexists_with).
narrative_ontology:cs_reading_relation('d7c476b6-1440-4cb5-a9d4-95e305499c9d', legal_personhood_boundary__functional_capacity_reading, coexists_with).
narrative_ontology:cs_axiom('d7c476b6-1440-4cb5-a9d4-95e305499c9d', foundational, born_humanity_requirement_is_definitional).
narrative_ontology:cs_axiom_status(born_humanity_requirement_is_definitional, holdable).
narrative_ontology:cs_axiom_grounding('d7c476b6-1440-4cb5-a9d4-95e305499c9d', born_humanity_requirement_is_definitional, conventional).
narrative_ontology:cs_axiom('d7c476b6-1440-4cb5-a9d4-95e305499c9d', secondary, cognitive_capacity_criterion_gates_membership_within_humans).
narrative_ontology:cs_axiom_status(cognitive_capacity_criterion_gates_membership_within_humans, holdable).
narrative_ontology:cs_axiom_grounding('d7c476b6-1440-4cb5-a9d4-95e305499c9d', cognitive_capacity_criterion_gates_membership_within_humans, empirically_contingent).
narrative_ontology:cs_reference_frame('d7c476b6-1440-4cb5-a9d4-95e305499c9d', enlightenment_individual_autonomy_doctrine).
narrative_ontology:cs_drift_state('d7c476b6-1440-4cb5-a9d4-95e305499c9d', contemporary_nonanthropocentric_challenge_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('d7c476b6-1440-4cb5-a9d4-95e305499c9d', '').
narrative_ontology:cs_kernel_id(legal_personhood_boundary__restrictive_anthropocentric_reading, legal_personhood_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legal_personhood_boundary__restrictive_anthropocentric_reading, pregnant_persons).
narrative_ontology:constraint_beneficiary(legal_personhood_boundary__restrictive_anthropocentric_reading, reproductive_autonomy_claimants).
narrative_ontology:constraint_beneficiary(legal_personhood_boundary__restrictive_anthropocentric_reading, cognitive_capacity_gatekeepers).
narrative_ontology:constraint_victim(legal_personhood_boundary__restrictive_anthropocentric_reading, fetuses).
narrative_ontology:constraint_victim(legal_personhood_boundary__restrictive_anthropocentric_reading, non_human_entities_claiming_rights).
narrative_ontology:constraint_victim(legal_personhood_boundary__restrictive_anthropocentric_reading, jurisdictions_recognizing_non_anthropocentric_standing).
narrative_ontology:constraint_vindicates(legal_personhood_boundary__restrictive_anthropocentric_reading, human_exceptionalism_doctrine).
narrative_ontology:constraint_vindicates(legal_personhood_boundary__restrictive_anthropocentric_reading, liberal_individualism_framework).
narrative_ontology:constraint_vindicates(legal_personhood_boundary__restrictive_anthropocentric_reading, cognitive_rationality_as_rights_criterion).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Their autonomous decision-making authority over reproduction is maximized by a personhood boundary that excludes the fetus from rights-bearing status. They can control their bodies and reproductive trajectories without legal interference grounded in fetal personhood claims. The constraint enables their liberty by denying the alternative reading's standing.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__restrictive_anthropocentric_reading, pregnant_persons, beneficiary,
    moderate, biographical, constrained, national).

% Advocacy coalitions and movements claiming rights to bodily autonomy, contraception access, and abortion without personhood-status interference. They articulate and defend the boundary; they benefit from its legal stability and from suppression of counter-readings that would restrict reproductive liberty.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__restrictive_anthropocentric_reading, reproductive_autonomy_claimants, beneficiary,
    organized, generational, mobile, national).

% Courts, legislatures, and jurisprudential authorities that define and adjudicate the personhood boundary. They enforce the cognitive capacity criterion and exclude entities that fail to meet it. Their authority depends on maintaining the stability of the boundary and suppressing alternative readings that would broaden the victim/rights-bearer set.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__restrictive_anthropocentric_reading, cognitive_capacity_gatekeepers, agenda_setter,
    institutional, generational, analytical, national).

% Excluded from the personhood set by this reading. They have no standing in courts, no independent legal claims, no voice in reproductive decisions that directly affect them. Their exclusion is structurally complete: they cannot organize, cannot testify, cannot challenge the boundary through legal process.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__restrictive_anthropocentric_reading, fetuses, payer,
    powerless, biographical, trapped, national).

% Entities like great apes, elephants, cetaceans with demonstrated cognitive capacity and sentience are excluded from the personhood set under this reading because they are not born humans. Even cognitive capacity does not secure standing if the human requirement is enforced. They remain property and subjects of use rather than rights-bearers, despite evidence of the capacities the reading nominally values.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__restrictive_anthropocentric_reading, non_human_animals_with_sentience, payer,
    powerless, biographical, trapped, global).
narrative_ontology:stakeholder_non_agent(legal_personhood_boundary__restrictive_anthropocentric_reading, non_human_animals_with_sentience).

% Excluded categorically by the born-human requirement. Even if they achieve demonstrable cognitive capacity exceeding human levels, they have no standing under this reading. The anthropocentric boundary forecloses their claim regardless of functional capacity.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__restrictive_anthropocentric_reading, artificial_intelligent_systems, payer,
    powerless, biographical, trapped, global).
narrative_ontology:stakeholder_non_agent(legal_personhood_boundary__restrictive_anthropocentric_reading, artificial_intelligent_systems).

% Parties who advocate for the developmental_potentiality_reading (personhood from conception). They claim fetuses are rights-bearers on the basis of human genetic continuity and life trajectory. They are structurally barred from full participation in reproductive law-making under this reading and must contest the boundary through political and legal contestation.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__restrictive_anthropocentric_reading, potentiality_rights_advocates, excluded,
    organized, generational, constrained, national).

% Parties claiming legal personhood for ecosystems, rivers, mountains, and species collectives on grounds of intrinsic value and complex systems properties. They are excluded from the personhood conversation entirely by the anthropocentric boundary. Jurisdictions recognizing environmental personhood (Ecuador's constitution, New Zealand river rights) represent competing readings the restrictive reading actively suppresses.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__restrictive_anthropocentric_reading, environmental_personhood_advocates, excluded,
    moderate, generational, constrained, global).

% Analytic observers and academic critics who examine the reading's coherence: does cognitive capacity actually track the entities the reading includes? Is the born-human requirement a residual essentialism or a defensible boundary? They produce counter-evidence (cetacean cognition, fetal neural development, AI language models) that strains the reading but does not displace it so long as enforcement holds.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__restrictive_anthropocentric_reading, bioethics_philosophers, observer,
    organized, generational, analytical, global).

% Jurisdictions and legal traditions that implement alternative readings. Indigenous legal systems recognizing ecosystem personhood, jurisdictions with potentiality-based abortion restrictions, emerging cases on AI rights. Their existence demonstrates that the restrictive reading is not inevitable law but one enforced choice among live alternatives.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__restrictive_anthropocentric_reading, comparative_legal_systems, observer,
    moderate, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(legal_personhood_boundary__restrictive_anthropocentric_reading, cognitive_capacity_gatekeepers).
narrative_ontology:fixing_cost_class(legal_personhood_boundary__restrictive_anthropocentric_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a stable, legally administrable boundary for rights-bearing status: one criterion (born human + cognitive capacity) that can be applied consistently across cases, avoiding the cost of case-by-case metaphysical inquiry into consciousness, potential, or intrinsic worth. Enables legal predictability for reproductive autonomy, property rights in animals, and AI regulation.
% TRANSFER_FUNCTION: Transfers the liberty to control reproduction and the bodies of pregnant persons from state custody (where personhood of the fetus would ground state interest) to pregnant persons themselves. Simultaneously transfers the status of non-human entities — animals, ecosystems, potential persons — from rights-bearers to objects of use and property. The constraint moves decisional authority over reproduction toward pregnant persons and away from institutional gatekeepers who would represent fetal interests; it moves ecosystems and animals further into the domain of human instrumental use.
% ABSENT_VOICES: Fetuses cannot speak in court or legislature. Non-human animals with documented cognition cannot advocate for themselves. Ecosystems and the interests of future generations have no direct legal voice. Potentiality-rights advocates are present in legislatures and courts but structurally marginalized by the boundary's enforcement — they are included in the political conversation but excluded from the status they claim for fetuses. Alternative-reading jurisdictions (Ecuador, New Zealand, indigenous systems) have been marginalized in comparative law by the dominance of Western liberal anthropocentric frameworks.
% DISAPPEARANCE_RATIONALE: If this reading's boundary were to vanish — replaced by potentiality-reading personhood from conception — reproductive law would reorganize entirely: abortion restrictions would widen, state interest in pregnancy would intensify, pregnant persons would lose autonomy. If replaced by functional-capacity reading, animals with demonstrated cognition would gain legal standing, environmental personhood would expand, AI systems with sophisticated language capacity might enter the rights domain. The constraint's disappearance would not leave society indifferent; it would trigger major reorganization of reproductive rights, animal law, environmental law, and emerging AI governance.
% FOUNDING_PROBLEM: Medieval and early modern law operated with poorly defined personhood criteria: did indigenous peoples count? Did women? Did enslaved persons? Did the unborn? The restrictive anthropocentric reading was constructed to provide a clear, administrable boundary that would exclude extension of personhood claims based on metaphysical speculation about potential or sentience, and would secure a stable zone for individual reproductive autonomy against state paternalism — the founding problem was 'how do we prevent infinite contestation of personhood status and protect reproductive liberty from state personhood-claims about fetuses.'
% FOUNDING_PROBLEM_CORROBORATION: Reproductive autonomy advocates and constitutional scholars attesting to the U.S. historical record (Roe v. Wade's personhood reasoning, state constitutions) corroborate that the founding problem (preventing fetal personhood from collapsing reproductive autonomy) was live. Potentiality-reading advocates contest whether the problem was real or whether it was manufactured to dodge moral obligations to the unborn. Bioethicists and comparative law scholars document that the problem is NOW actively contested: jurisdictions are adopting alternative readings (Ecuador, New Zealand on ecosystems; Poland, Hungary on potentiality), which shows the founding problem's status is being actively revisited rather than settled.
narrative_ontology:disappearance_verdict(legal_personhood_boundary__restrictive_anthropocentric_reading, world_rearranges).
narrative_ontology:founding_problem_status(legal_personhood_boundary__restrictive_anthropocentric_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legal_personhood_boundary__restrictive_anthropocentric_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(legal_personhood_boundary__restrictive_anthropocentric_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legal_personhood_boundary__restrictive_anthropocentric_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legal_personhood_boundary__restrictive_anthropocentric_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(legal_personhood_boundary__restrictive_anthropocentric_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(legal_personhood_boundary__restrictive_anthropocentric_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68 at interval end) is substantial because the reading directly denies rights-bearing status to entities (fetuses, sentient animals, potentially-person-bearing systems) who would have significant claims under alternative readings, concentrating decisional authority in the hands of institutions and beneficiaries of the anthropocentric framework. Suppression (0.71) is high because maintaining the boundary requires active exclusion of voice from potentiality advocates and alternative-reading jurisdictions; the boundary is not self-evident but must be defended. Theater_ratio (0.44 at interval end, rising from 0.28) indicates that a growing share of boundary-enforcement activity is performative — courts and legislators increasingly invoke cognitive capacity as the criterion while simultaneously excluding non-human animals with documented equal or superior cognitive capacity (great apes, elephants, cetaceans), revealing the criterion is not the actual enforcer; the born-human requirement is. Accessibility_collapse (0.62) reflects that once a person is not born, the reading offers no route to personhood status regardless of capacity; for non-humans, the born-human gate is absolute. Resistance (0.58) shows moderate but real pushback: potentiality-reading jurisdictions exist, environmental law is developing alternative frameworks, animal law advocates document cognitive parity, and AI ethicists question the boundary. The measurement series track the constraint's operation across a 40-year window (1985-2025 approximate), showing slow extraction accumulation and increasing theater as empirical challenges to the boundary mount but institutional suppression persists.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seats and the victim seats compute radically different classifications. From the pregnant-person and autonomy-coalition seats, the constraint is genuine coordination with minor enforcement overhead — a stable boundary. From the fetus and non-human-animal seats, it is pure extraction masked by a coordinate frame. The potentiality-advocate seat experiences suppression (their alternative reading is active but marginalized). The cognitive-capacity-gatekeeper seat experiences the constraint as rule-setting, not extraction, though their institutional power to enforce the boundary is the precise mechanism of extraction for other seats. The engine computes these divergences from the structural data; the authored claim (tangled_rope) does not adjudicate them but identifies the constraint's type as hybrid (coordination + extraction requiring enforcement).
 *
 * DIRECTIONALITY LOGIC:
 *   Pregnant persons (moderate power, constrained exit, national scope) are beneficiaries with low directionality (d ≈ 0.25): they collect reproductive autonomy and are relatively mobile within legal jurisdictions (can relocate for access if constrained in home jurisdiction). Reproductive autonomy coalitions (organized power, mobile exit) have even lower d (≈ 0.15): they actively defend the boundary and can exit to favorable jurisdictions. Cognitive capacity gatekeepers (institutional power, analytical exit) are the agenda-setters with d approaching 0.0 — they set the boundary, are never targets of extraction, and have analytical-level exit (they can alter the rule entirely). Fetuses (powerless, trapped exit, no voice) have maximum d (≈ 1.0): they are the primary extraction target, cannot organize, cannot leave, cannot contest the boundary through legal process. Non-human animals (powerless to organized, trapped exit) have high d (≈ 0.8-0.9): the born-human gate denies them status regardless of capacity, and they have no voice. Potentiality-rights advocates (organized, constrained exit) have elevated d (≈ 0.55-0.65): they pay the cost of having their reading marginalized and bear the burden of contestation, though they retain some institutional presence.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing infinite contestation of personhood status and protecting reproductive liberty from fetal-personhood claims) remains LIVE in the sense that jurisdictions continue actively enforcing this reading. However, the problem's NECESSITY is increasingly contested: potentiality-reading jurisdictions show that alternative boundaries do not cause societal chaos, and environmental/animal law develops without collapsing under personhood-expansion. The theater_ratio trajectory (rising from 0.28 to 0.44) indicates growing mandate drift: the original coordination function (stable, administrable boundary) persists, but an increasing share of enforcement activity is devoted to suppressing alternatives rather than solving the founding problem. The constraint is not yet a piton (it retains genuine coordination value and organized beneficiaries), but the theater_ratio rise signals early mandatrophy. If the trajectory continues and theater approaches 0.6+, the constraint would approach piton status (atrophied founding function, persistence by inertia and suppression rather than genuine coordination need).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cognitive_capacity_criterion_instability,
    'Does the stated cognitive capacity criterion actually govern the boundary, or is the born-human requirement the de facto gate that overrides capacity in practice?',
    'Examine case law and regulatory practice: do non-human animals or AI systems with documented cognitive capacity equal to or exceeding human levels ever gain personhood status or standing under this reading? If no, the criterion is theater; the gate is birth-species membership. If yes, the criterion operates as stated.',
    'If the criterion is theater (cognitive capacity invoked but not determinative), the constraint is more extractive and suppressive than the stated logic suggests — it is rank exclusion masked by a false criterion. This would increase ε and shift type toward pure snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(cognitive_capacity_criterion_instability, empirical, 'Whether cognitive capacity is the actual criterion or a cover for anthropocentric rank exclusion.').

omega_variable(
    potentiality_vs_actuality_boundary_necessity,
    'Is the denial of fetal personhood based on actual (born status, current capacity) or potential (future capacity, genetic continuity) properties? If actual, why is the boundary necessary to protect reproductive autonomy — could a functional-capacity reading that includes mature fetuses also protect autonomy through a different route?',
    'Comparative law analysis: jurisdictions with potentiality-based personhood (e.g., Poland) do restrict abortion but do not necessarily eliminate reproductive autonomy entirely (medical exception, some health-based access). The necessity of THIS reading''s boundary is contested by these examples.',
    'If potentiality-based personhood does not inherently eliminate reproductive autonomy (only restricts its scope), then this reading''s claim to coordinate reproductive liberty is overstated — it coordinates not autonomy but maximal autonomy, which is a preference rather than a necessity. This would lower the claimed coordination value and increase the relative share of extraction (preferential access for autonomy beneficiaries over potentiality-bearing entities).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(potentiality_vs_actuality_boundary_necessity, conceptual, 'Whether the boundary is structurally necessary for reproductive autonomy or a preference for maximal autonomy that forecloses alternative readings.').

omega_variable(
    anthropocentric_essentialism_vs_functional_coherence,
    'Is the born-human requirement a defensible boundary criterion, or is it an essentialism residue that survives only because enforcement suppresses challenges?',
    'Philosophical and empirical analysis: if the reading''s core claim is that cognitive capacity matters (sentience, self-awareness, rationality), then entities with those capacities should gain standing regardless of birth or species. If the born-human requirement overrides that claim, the reading is inconsistent — the criterion does not explain the boundary. The inconsistency would only persist if enforcement suppresses the logical challenge.',
    'If the boundary is essentialist (resting on human-identity rather than the stated functional criterion), then the constraint is fundamentally extractive for entities that have the functional properties but lack the essential property. This would support reclassification to snare, and would increase ε substantially if the inconsistency became publicly undeniable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(anthropocentric_essentialism_vs_functional_coherence, conceptual, 'Whether the born-human requirement is coherent with the reading''s stated functional criterion or an essentialism that contradicts it.').

omega_variable(
    kernel_reading_displacement_timeline,
    'Over what timescale, if any, would the functional_capacity_reading or developmental_potentiality_reading displace this restrictive reading as the legal standard in major jurisdictions?',
    'Jurisdictional monitoring: track adoption of alternative readings in legislatures, courts, and constitutional amendments. Watch for cases that grant standing to non-human animals or ecosystems on functional-capacity grounds, or for expanding personhood at conception in potentiality-reading jurisdictions. Observe whether enforcement of THIS reading strengthens or weakens relative to challenges.',
    'A rapid displacement (within 10-15 years) would suggest the reading lacks structural stability and rests primarily on temporary institutional configuration rather than deep normative consensus. This would increase theater_ratio projections and move the constraint toward piton. A stable trajectory would suggest the reading retains genuine institutional support and coordination value.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_displacement_timeline, empirical, 'Whether this reading will maintain institutional dominance or be displaced by a sibling reading over the medium term.').

omega_variable(
    reproductive_autonomy_benignity_coupling,
    'Does the restriction of fetal personhood actually require excluding potentiality-bearing entities from all legal standing, or could a compromise framework grant fetal interests some consideration while preserving pregnant-person decisional autonomy?',
    'Jurisdictional examples and policy analysis: some potentiality-reading jurisdictions (e.g., Germany) grant fetal interests some legal weight but permit abortion in early pregnancy, suggesting decoupling is possible. Examine whether decoupling would preserve the coordination value of reproductive autonomy without requiring total fetal-personhood denial.',
    'If decoupling is viable, then the reading''s beneficiary structure is narrower than necessary — it extracts from fetuses more than autonomy-coordination requires. This would increase the assessed extraction relative to coordination and lower the coordination-type classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reproductive_autonomy_benignity_coupling, conceptual, 'Whether reproductive autonomy requires total fetal depersonhood or whether compromise frameworks exist that couple autonomy with partial fetal standing.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legal_personhood_boundary__restrictive_anthropocentric_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lega_tr_t0, legal_personhood_boundary__restrictive_anthropocentric_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(lega_tr_t0, observed).
narrative_ontology:measurement(lega_tr_t8, legal_personhood_boundary__restrictive_anthropocentric_reading, theater_ratio, 8, 0.32).
narrative_ontology:measurement_basis(lega_tr_t8, observed).
narrative_ontology:measurement(lega_tr_t16, legal_personhood_boundary__restrictive_anthropocentric_reading, theater_ratio, 16, 0.37).
narrative_ontology:measurement_basis(lega_tr_t16, observed).
narrative_ontology:measurement(lega_tr_t24, legal_personhood_boundary__restrictive_anthropocentric_reading, theater_ratio, 24, 0.4).
narrative_ontology:measurement_basis(lega_tr_t24, observed).
narrative_ontology:measurement(lega_tr_t32, legal_personhood_boundary__restrictive_anthropocentric_reading, theater_ratio, 32, 0.42).
narrative_ontology:measurement_basis(lega_tr_t32, observed).
narrative_ontology:measurement(lega_tr_t40, legal_personhood_boundary__restrictive_anthropocentric_reading, theater_ratio, 40, 0.44).
narrative_ontology:measurement_basis(lega_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(lega_be_t0, legal_personhood_boundary__restrictive_anthropocentric_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement_basis(lega_be_t0, observed).
narrative_ontology:measurement(lega_be_t8, legal_personhood_boundary__restrictive_anthropocentric_reading, base_extractiveness, 8, 0.59).
narrative_ontology:measurement_basis(lega_be_t8, observed).
narrative_ontology:measurement(lega_be_t16, legal_personhood_boundary__restrictive_anthropocentric_reading, base_extractiveness, 16, 0.63).
narrative_ontology:measurement_basis(lega_be_t16, observed).
narrative_ontology:measurement(lega_be_t24, legal_personhood_boundary__restrictive_anthropocentric_reading, base_extractiveness, 24, 0.66).
narrative_ontology:measurement_basis(lega_be_t24, observed).
narrative_ontology:measurement(lega_be_t32, legal_personhood_boundary__restrictive_anthropocentric_reading, base_extractiveness, 32, 0.67).
narrative_ontology:measurement_basis(lega_be_t32, observed).
narrative_ontology:measurement(lega_be_t40, legal_personhood_boundary__restrictive_anthropocentric_reading, base_extractiveness, 40, 0.68).
narrative_ontology:measurement_basis(lega_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(lega_su_t0, legal_personhood_boundary__restrictive_anthropocentric_reading, suppression_requirement, 0, 0.62).
narrative_ontology:measurement_basis(lega_su_t0, observed).
narrative_ontology:measurement(lega_su_t8, legal_personhood_boundary__restrictive_anthropocentric_reading, suppression_requirement, 8, 0.65).
narrative_ontology:measurement_basis(lega_su_t8, observed).
narrative_ontology:measurement(lega_su_t16, legal_personhood_boundary__restrictive_anthropocentric_reading, suppression_requirement, 16, 0.68).
narrative_ontology:measurement_basis(lega_su_t16, observed).
narrative_ontology:measurement(lega_su_t24, legal_personhood_boundary__restrictive_anthropocentric_reading, suppression_requirement, 24, 0.7).
narrative_ontology:measurement_basis(lega_su_t24, observed).
narrative_ontology:measurement(lega_su_t32, legal_personhood_boundary__restrictive_anthropocentric_reading, suppression_requirement, 32, 0.71).
narrative_ontology:measurement_basis(lega_su_t32, observed).
narrative_ontology:measurement(lega_su_t40, legal_personhood_boundary__restrictive_anthropocentric_reading, suppression_requirement, 40, 0.71).
narrative_ontology:measurement_basis(lega_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legal_personhood_boundary__restrictive_anthropocentric_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(legal_personhood_boundary__restrictive_anthropocentric_reading, 0.12).
narrative_ontology:affects_constraint(legal_personhood_boundary__restrictive_anthropocentric_reading, legal_personhood_boundary__developmental_potentiality_reading).
narrative_ontology:affects_constraint(legal_personhood_boundary__restrictive_anthropocentric_reading, legal_personhood_boundary__functional_capacity_reading).
narrative_ontology:affects_constraint(legal_personhood_boundary__restrictive_anthropocentric_reading, reproductive_autonomy_enforcement_mechanism).
narrative_ontology:affects_constraint(legal_personhood_boundary__restrictive_anthropocentric_reading, animal_legal_standing_restriction).
narrative_ontology:affects_constraint(legal_personhood_boundary__restrictive_anthropocentric_reading, environmental_personhood_exclusion).

% DUAL FORMULATION NOTE:
% The legal_personhood_boundary kernel has three structurally distinct constraint instantiations, each a different reading of the same persisting commitment to defining rights-bearing status. The restrictive_anthropocentric_reading is one enforced configuration within a contested kernel. The sibling readings (developmental_potentiality_reading and functional_capacity_reading) are separate constraint stories with different ε values, different victim sets, different enforcement patterns. Each reading couples personhood definition to adjacent legal domains (reproductive law, animal law, environmental law, AI governance); the network links capture these structural influences. Constraint families decomposed per ε-invariance principle (OQ-26): if the observable used to evaluate personhood status (birth? cognitive capacity? genetic continuity?) changes ε materially, the observer is looking at a different constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(legal_personhood_boundary__restrictive_anthropocentric_reading, powerless, 1.0).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
