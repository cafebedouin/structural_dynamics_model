% ============================================================================
% CONSTRAINT STORY: jurisprudential_method_kernel__hanbali_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jurisprudential_method_kernel__hanbali_reading, []).

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
 *   constraint_id: jurisprudential_method_kernel__hanbali_reading
 *   human_readable: Strict Textual Derivation Rule — Hanbali Reading of the Jurisprudential Method Kernel
 *   domain: religious/legal/institutional
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the jurisprudential-method kernel:
 *   the strict textualist rule under which valid law derives only from the
 *   literal text of scripture and report, the precedent of the founding
 *   generation, and unanimous consensus, with analogical reasoning and
 *   juristic preference condemned as corrupting innovation. The colloquial
 *   label 'Sunni legal methodology' covers four structurally distinct claims,
 *   decomposed per the epsilon-invariance principle into four linked stories:
 *   this one (hanbali_reading), plus the hanafi, maliki, and shafii readings
 *   as separate constraints. Epsilon differs sharply across the family: under
 *   the hanafi reading the rationalist method is the beneficiary structure
 *   and epsilon drops; under the maliki reading customary practice gains a
 *   validity channel; under the shafii reading extraction is bounded by a
 *   controlled fourth tier. This reading carries the family's highest epsilon
 *   because it uniquely condemns the shared instrument itself rather than
 *   ranking or absorbing it. The epsilon referent here is the standing
 *   arrangement under contest — the strict derivation regime as it has
 *   actually operated from the school's formation to the present — assessed
 *   from the analytical seat; the reading's endorsed ideal (pure text-only
 *   practice) is NOT the referent. The claim/metric gap is deliberate: the
 *   arrangement is claimed as tangled_rope (genuine textual-anchor
 *   coordination plus asymmetric authority concentration) and the metrics are
 *   authored from its observed operation; the engine computes per-seat
 *   classifications from the structural data and the divergence, if any, is
 *   the measurement the corpus exists to take.
 *
 * KEY AGENTS:
 *   - hanbali_methodological_authorities: agenda-setter and principal beneficiary seat (institutional/identity_locked) — administers the validity boundary, collects the certification rent
 *   - hadith_transmission_scholars: primary beneficiary (powerful/identity_locked) — transmission mastery is the sole currency of authority under the rule
 *   - rationalist_jurists: primary target (organized/constrained) — their method is pre-classified as corruption; exit costs career and standing
 *   - local_custom_communities: primary target (moderate/trapped) — inherited practice delegitimated where ungrounded in text or unanimity
 *   - lay_muslims: dual-positioned (organized/identity_locked) — genuine verification benefit, real rigidity cost, near-symmetric position
 *   - rival_madhhab_jurists: excluded seat (organized/mobile) — pre-classified rather than answered; hold live alternatives elsewhere
 *   - state_religious_establishments: late-entering agenda-setter and beneficiary (institutional/identity_locked) — enforcement capacity and legitimacy claim fused with the method since the Najdi compact
 *   - comparative_usul_historians: analytical observer (analytical/analytical) — sees the full four-reading structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jurisprudential_method_kernel__hanbali_reading, 0.78).
domain_priors:suppression_score(jurisprudential_method_kernel__hanbali_reading, 0.72).
domain_priors:theater_ratio(jurisprudential_method_kernel__hanbali_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanbali_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanbali_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanbali_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanbali_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanbali_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jurisprudential_method_kernel__hanbali_reading, tangled_rope).
narrative_ontology:human_readable(jurisprudential_method_kernel__hanbali_reading, "Strict Textual Derivation Rule — Hanbali Reading of the Jurisprudential Method Kernel").
narrative_ontology:topic_domain(jurisprudential_method_kernel__hanbali_reading, "religious/legal/institutional").

domain_priors:requires_active_enforcement(jurisprudential_method_kernel__hanbali_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jurisprudential_method_kernel__hanbali_reading, 'e0420a54-f47a-4e42-9c32-92ee68fed0d8').
narrative_ontology:cs_kernel_codification('e0420a54-f47a-4e42-9c32-92ee68fed0d8', fixed_text).
narrative_ontology:cs_authority_grounding('e0420a54-f47a-4e42-9c32-92ee68fed0d8', lineage).
narrative_ontology:cs_interpretation_layer_present('e0420a54-f47a-4e42-9c32-92ee68fed0d8').
narrative_ontology:cs_reading_relation('e0420a54-f47a-4e42-9c32-92ee68fed0d8', jurisprudential_method_kernel__hanafi_reading, forecloses).
narrative_ontology:cs_reading_relation('e0420a54-f47a-4e42-9c32-92ee68fed0d8', jurisprudential_method_kernel__maliki_reading, coexists_with).
narrative_ontology:cs_reading_relation('e0420a54-f47a-4e42-9c32-92ee68fed0d8', jurisprudential_method_kernel__shafii_reading, influences).
narrative_ontology:cs_axiom('e0420a54-f47a-4e42-9c32-92ee68fed0d8', foundational, analogical_extension_is_bidah).
narrative_ontology:cs_axiom_status(analogical_extension_is_bidah, holdable).
narrative_ontology:cs_axiom_grounding('e0420a54-f47a-4e42-9c32-92ee68fed0d8', analogical_extension_is_bidah, theological).
narrative_ontology:cs_axiom('e0420a54-f47a-4e42-9c32-92ee68fed0d8', foundational, valid_sources_closed_at_text_and_companions).
narrative_ontology:cs_axiom_status(valid_sources_closed_at_text_and_companions, holdable).
narrative_ontology:cs_axiom_grounding('e0420a54-f47a-4e42-9c32-92ee68fed0d8', valid_sources_closed_at_text_and_companions, theological).
narrative_ontology:cs_axiom('e0420a54-f47a-4e42-9c32-92ee68fed0d8', secondary, only_unanimity_binds).
narrative_ontology:cs_axiom_status(only_unanimity_binds, holdable).
narrative_ontology:cs_axiom_grounding('e0420a54-f47a-4e42-9c32-92ee68fed0d8', only_unanimity_binds, theological).
narrative_ontology:cs_reference_frame('e0420a54-f47a-4e42-9c32-92ee68fed0d8', salaf_textual_derivation_purity).
narrative_ontology:cs_drift_state('e0420a54-f47a-4e42-9c32-92ee68fed0d8', contemporary_institutionalized_orthodoxy, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e0420a54-f47a-4e42-9c32-92ee68fed0d8', '').
narrative_ontology:cs_kernel_id(jurisprudential_method_kernel__hanbali_reading, jurisprudential_method_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__hanbali_reading, hadith_transmission_scholars).
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__hanbali_reading, hanbali_methodological_authorities).
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__hanbali_reading, state_religious_establishments).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__hanbali_reading, rationalist_jurists).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__hanbali_reading, local_custom_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__hanbali_reading, lay_muslims).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__hanbali_reading, lay_muslims).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Senior scholars of the school who define which derivation moves count as valid, certify jurists, issue authoritative rulings, and pronounce the condemnation of innovation when methodological rivals press their claims. Their standing is constituted by the method itself: an authority who conceded that reasoned extension is a legitimate source would dissolve the basis of his own certification role. Historically centered in Baghdad, Damascus, and Najd; today anchored in state-linked academies and fatwa bodies with global reach.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanbali_reading, hanbali_methodological_authorities, agenda_setter,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(jurisprudential_method_kernel__hanbali_reading, hanbali_methodological_authorities, beneficiary).

% Specialists in the chains and texts of prophetic report. Under the strict rule their mastery is the sole currency of legal authority: a jurist who can cite and weigh reports outranks one who argues from analogy. Generations of training, reputations, and endowed teaching posts are invested in transmission skill; a turn that admitted reasoned extension as an equal source would devalue that accumulated capital. They can take positions within the school but not against its textual anchor without forfeiting their standing.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanbali_reading, hadith_transmission_scholars, beneficiary,
    powerful, generational, identity_locked, continental).

% Jurists trained in systematic reasoning — analogy, equity-preference, purposive extension — whose toolkit the rule classifies as corrupting innovation rather than as method. Within jurisdictions and institutions governed by the strict rule, their reasoning is inadmissible for establishing law however rigorous. Their options: retrain into textualist method at cost to career and standing, practice under rival schools in other regions, or work in advisory fields such as finance and medicine where their reasoning survives without binding force. Historically the Hanafi methodologists of Abbasid Baghdad and the rationalist currents they contested.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanbali_reading, rationalist_jurists, payer,
    organized, biographical, constrained, continental).

% Communities whose commerce, marriage, water-sharing, and inheritance arrangements run on inherited local practice. Practice that cannot be traced to report or to agreement of the founding generation is reclassified as suspect innovation when it conflicts with textual rulings, and receives no validity channel of its own under a unanimity standard that is practically unattainable. They cannot exit the jurisdiction of the law that overrides their custom; their recourse is compliance or quiet concealment of practice.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanbali_reading, local_custom_communities, payer,
    moderate, generational, trapped, regional).

% Non-specialist believers who receive a law anchored in a fixed, publicly checkable text: any expert claim can be challenged with 'show me the report,' which disciplines juristic caprice and gives non-experts a verification handle. They pay in rigidity: novel situations — financial instruments, medical procedures — that the text does not address fall into a gray zone where the strict rule offers no lawful derivation path, and the customary accommodations they actually live by carry stigma. Exit from the faith community is not a live option for most; contestation happens inside.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanbali_reading, lay_muslims, beneficiary,
    organized, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(jurisprudential_method_kernel__hanbali_reading, lay_muslims, payer).

% Jurists of the other schools of law, whose methods the strict rule pre-classifies as innovation rather than engaging as argument. They hold their own institutions, courts, and teaching lines in other regions and can move between them; what they cannot do is enter the strict framework's validity conversation, since its rules define their method as corruption rather than as difference.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanbali_reading, rival_madhhab_jurists, excluded,
    organized, biographical, mobile, continental).

% The modern state, from the eighteenth-century Najdi compact onward, that adopted the strict textual method as official doctrine and enforces it through courts, education ministries, morality policing, and international propagation. Legitimacy flows to the state from upholding the kernel; the same entanglement means the state cannot relax the method without touching its own legitimacy claim. Its regulatory bodies nonetheless reason purposively in novel domains, a tension managed below the level of official doctrine.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanbali_reading, state_religious_establishments, agenda_setter,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(jurisprudential_method_kernel__hanbali_reading, state_religious_establishments, beneficiary).

% Scholars of legal theory and institutional history who study all four readings of the methodological kernel side by side. They see the shared kernel, the divergent source-validity rules, and where each reading concentrates authority and cost; they attest and collect nothing within the arrangement itself.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanbali_reading, comparative_usul_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jurisprudential_method_kernel__hanbali_reading, hanbali_methodological_authorities).
narrative_ontology:fixing_cost_class(jurisprudential_method_kernel__hanbali_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Anchors legal derivation to a stable, publicly verifiable textual corpus shared by the whole community: it solves the problem of who decides what divine law requires by tying the answer to fixed text and founding-generation precedent instead of to contestable reasoning skill, gives non-specialists a verification handle against expert caprice, and raises the cost of fabricating religious authority.
% TRANSFER_FUNCTION: Moves hermeneutic authority and law-establishing legitimacy from jurists whose currency is reasoning skill to jurists whose currency is transmission mastery; moves customary practice from the category of law to the category of suspect innovation; and moves the power to declare what counts as law to whoever certifies textual fidelity — historically from rationalist methodologists to hadith scholars and certification authorities, and in the modern era substantially to the state that upholds the method.
% ABSENT_VOICES: Rationalist jurists, customary-law communities, and rival-school jurists are structurally outside the framework's validity conditions: within the reading their objections are pre-classified as innovation rather than heard as methodological argument. Lay believers' practical legal needs in novel domains have no seat inside the strict method either — the people whose lives the unregulated gray zones touch are not in the derivation conversation.
% DISAPPEARANCE_RATIONALE: If the strict rule vanished overnight, legal derivation would reorganize around the rival methods it excludes: reasoned extension would re-enter as a lawful source, customary practice would regain a validity channel, and the authority economy built on transmission mastery and certification would have to compete on argument. The other three readings' arrangements, and the modern state's legitimacy claim that rides on this one, all depend on it holding.
% FOUNDING_PROBLEM: In the formative centuries, legal derivation had proliferated: jurists in Kufa and Baghdad reasoned freely from analogy and personal preference, producing divergent rulings all attributed to divine law, while reports of the Prophet circulated with weak or fabricated chains deployed opportunistically. The founding problem: how to bind law to divine authority when juristic reason is fallible and interests corrupt — answered by restricting valid sources to text, Companion precedent, and unanimous consensus, and condemning everything else as innovation.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is corroborated from outside the benefiting parties: al-Shafi'i — authority of a sibling reading, not a beneficiary of this one — attacked unbounded juristic preference and weak report in his methodological treatise, confirming the problem was real and shared across the tradition. The status is attested as contested by the survival and arguments of the sibling schools, by the school's own later methodological manuals (which admit controlled analogy), and by comparative scholarship on the formative period; no party outside the textualist beneficiary set attests the strict solution as the uniquely valid answer.
narrative_ontology:disappearance_verdict(jurisprudential_method_kernel__hanbali_reading, world_rearranges).
narrative_ontology:founding_problem_status(jurisprudential_method_kernel__hanbali_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jurisprudential_method_kernel__hanbali_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(jurisprudential_method_kernel__hanbali_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jurisprudential_method_kernel__hanbali_reading, 0.78, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jurisprudential_method_kernel__hanbali_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jurisprudential_method_kernel__hanbali_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jurisprudential_method_kernel__hanbali_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is high (0.78 at interval end) because the rule converts a methodological disagreement into a validity verdict: rivals are not out-argued but pre-classified, and the authority their exclusion frees accrues to the certification seats. Suppression is high (0.72) and its enforcement machinery is the story's tracked dynamic — the series runs on one shared time grid (855, 925, 1258, 1328, 1744, 1932, 2024) with every metric authored at every point, so the enforcement trajectory is explicit: doctrinal polemic at formation, street-level enforcement under al-Barbahari, a dip as the school dispersed and reconstituted after the Mongol sack, then a step-change to state-backed coercion with the Najdi compact and the modern kingdom, with a slight recent easing as enforcement changed form (institutional and platform enforcement replacing street enforcement) rather than decaying. Theater rises from near-zero to 0.45: the textual discipline was almost pure function at formation, but a growing share of maintenance is now performative — official no-reasoning rhetoric coexists with purposive reasoning in regulatory practice, and purity declaration continues while practice accommodates. Accessibility_collapse is 0.58: within governed jurisdictions the alternatives (analogy, custom) collapse nearly completely as legal channels, but exit to other schools and regions historically preserved alternatives, so collapse is partial rather than total. Resistance is 0.6: rival-school polemic across twelve centuries, the school's own internal accommodation of analogy in its later methodological manuals, and modern reformist critique. Suppression is authored as a raw structural property and is not scaled by power or scope; only extractiveness is scaled, by directionality and scope, in the engine's computation.
 *
 * PERSPECTIVAL GAP:
 *   The payer and beneficiary seats should compute very different types from the same structural data. From the certification seat the arrangement is the protection of divine law from human invention — coordination it stewards, with the unanimity standard as rigor rather than gatekeeping. From the rationalist jurist seat the same structure is enforced exclusion from law-making: their training is ruled inadmissible, not refuted. From the custom communities it is delegitimation of inherited law they cannot exit. The excluded rival-school seats experience pre-classification rather than argument. The lay seat sits between: real verification benefit, real rigidity cost. The engine computes this divergence per seat from power, exit, and role data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   The certification authorities, transmission scholars, and state establishment are structural beneficiaries (d near the beneficiary end: the rule subsidizes their authority position, and their identity_lock means they cannot reposition without dissolving their standing). Rationalist jurists (constrained exit) and custom communities (trapped) are targets (d near the full-target end). Lay Muslims sit near symmetric: genuine anchor-and-verification benefit against rigidity and stigma costs. No directionality_overrides are authored: the beneficiary/victim declarations plus exit options already separate the seats, and the override surface is keyed by power atom rather than agent, so an override for the near-symmetric organized lay seat would misfire onto the organized payer seat (rationalist jurists) whose derived high d is correct.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — binding law to divine authority against juristic caprice and fabricated report — remains live in mutated form, so the mandate has not fully outlived its function; but the strict mechanism has partially outlived its specific solution: the unanimity standard is practically unattainable after the first generations and functions as a gate that keeps law-creation authority with the certifiers, and analogical reasoning returns in practice under other names while the no-analogy rhetoric continues. The tangled_rope classification prevents two mislabels: a pure extraction reading would miss the genuine coordination (a fixed publicly checkable anchor, anti-fabrication discipline, the lay verification handle), and a pure coordination reading would miss the asymmetric authority concentration and the delegitimation of custom that ride on the same structure. Persistence is now maintained substantially by identity fusion (the certifiers' standing IS the method) and state entanglement (the legitimacy claim is fused with it) as much as by solving the founding problem.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This constraint is one reading (hanbali_reading) of the jurisprudential_method_kernel; what structurally changes if a sibling reading is instantiated instead, and where exactly is the disagreement located?',
    'Compare the four linked stories'' beneficiary/victim structures and epsilon values: under the hanafi reading rationalist jurists move from victims to beneficiaries and epsilon drops; under the maliki reading local custom gains a validity channel; under the shafii reading extraction is bounded by the controlled fourth tier. The disagreement is located at the source-validity rule: whether human reason and living practice can ground law or only transmitted text and founding-generation precedent.',
    'The victim set, the directionality map, and the classification all shift with the reading; cross-reading comparisons that treat the four stories as one constraint will average away exactly the structural signal the decomposition exists to preserve.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Committer structure: which kernel, which reading, what siblings would change, where the disagreement sits.').

omega_variable(
    strict_strand_scoping,
    'Does this reading describe the strict early strand of the school only, or the school''s whole history? The school''s own later methodological manuals admit controlled analogy, and its greatest classical figure reasoned purposively while maintaining textualist rhetoric.',
    'Textual-historical scoping: date the school''s internal admission of analogy in its methodological literature and decide whether the manifest''s reading instantiates the pre-accommodation strict strand or the school as historically continuous.',
    'If scoped to the whole school history, the no-analogy axiom is partially overridden within the reading''s own tradition, epsilon drops, and the drift_state direction shifts toward acknowledged internal revision; if scoped to the strict strand, the authored values stand.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(strict_strand_scoping, empirical, 'Scoping ambiguity between the strict strand and the school''s accommodative later history.').

omega_variable(
    unanimity_gatekeeping,
    'Is the unanimity-only consensus standard a genuine validity condition or a gatekeeping device, given that unanimous agreement is practically unestablishable after the founding generations?',
    'Examine the historical record of claimed consensus: if accepted claims of consensus track the certification authorities'' interests and near-unanimous agreement is rejected, the standard functions as a gate that reserves law-creation for the text-certifiers; if genuine broad agreement is routinely accepted, it is a real if strict validity condition.',
    'If gatekeeping, a large share of the measured extraction is produced by the unanimity rule specifically, and the coordination function of the consensus tier is largely nominal — pushing the arrangement toward the extraction-heavy end of the hybrid range.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unanimity_gatekeeping, empirical, 'Whether the unanimity standard binds or gates.').

omega_variable(
    internalized_bidah_stigma,
    'Is the suppression of rationalist method purely structural (exclusion from courts, certification, and career paths) or partly internalized — do jurists trained under the regime pre-censor their own reasoning as corrupt, carrying the stigma after institutional exit?',
    'Post-exit trajectory: study jurists who moved to rival-school institutions or advisory fields — if self-restriction of analogical argument persists where no enforcement reaches it, part of the suppression is internalized.',
    'If internalized, the constraint''s effective suppression exceeds the structural measure: the regime reproduces itself through the trained intuitions of its targets, and enforcement-capacity decay would not immediately release the suppressed method.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(internalized_bidah_stigma, empirical, 'Structural versus internalized suppression mechanism for rationalist-trained jurists.').

omega_variable(
    custom_exclusion_scope,
    'How much of the constraint''s cost actually lands on customary practice, given that all schools including this one formally recognize custom as a secondary evidence source where it does not conflict with text?',
    'Case-level analysis of custom conflicts: measure whether ungrounded but uncontested custom is tolerated in practice or overridden, and whether the unanimity requirement leaves custom with any workable validity channel.',
    'If custom retains a workable channel, the victim declaration overstates the custom-side extraction and the effective victim set narrows toward rationalist jurists; if the channel is nominal, the custom communities'' trapped-target directionality stands at full strength.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(custom_exclusion_scope, empirical, 'Scope of the custom-delegitimation component of the extraction.').

omega_variable(
    covert_analogy_theater,
    'Does official practice under the modern institutionalized form reason analogically under other names (purposive interest analysis, regulatory committees, novel-domain rulings) while maintaining the no-analogy rhetoric?',
    'Documentary comparison of official methodological declarations against the actual reasoning structure of contemporary regulatory and novel-domain rulings issued by institutions formally committed to the strict rule.',
    'If the gap is wide, the theater_ratio understates performative maintenance in the modern era and the arrangement''s contemporary operation moves toward theatrical maintenance of a kernel its operators no longer practice — a piton-direction pressure on the modern form.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(covert_analogy_theater, empirical, 'Whether the no-analogy rhetoric is maintained performatively over analogical practice.').

omega_variable(
    madhhab_pluralism_metaframe,
    'Does the Sunni meta-doctrine of tolerated methodological diversity — under which all four readings were historically held as simultaneously valid — dissolve the authored foreclosure relation between this reading and the hanafi reading?',
    'Conceptual analysis at the meta-framework level: determine whether the pluralist meta-doctrine constitutes a single framework in which both readings coexist (dissolving foreclosure into coexistence) or whether this reading''s exclusivist core premise (''analogy is corrupting innovation'') is incompatible with the meta-doctrine itself, leaving the foreclosure intact.',
    'If the meta-frame dissolves the foreclosure, the reading-relations edge to the hanafi reading should be coexists_with and the cross-reading contamination analysis changes shape; if the exclusivist core stands, the foreclosure is real and the pluralist meta-doctrine is itself one of the things this reading suppresses.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(madhhab_pluralism_metaframe, conceptual, 'Whether the ikhtilaf-tolerance meta-framework overrides the authored foreclosure edge.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jurisprudential_method_kernel__hanbali_reading, 855, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hanbali_reading_tr_t855, jurisprudential_method_kernel__hanbali_reading, theater_ratio, 855, 0.1).
narrative_ontology:measurement(hanbali_reading_tr_t925, jurisprudential_method_kernel__hanbali_reading, theater_ratio, 925, 0.15).
narrative_ontology:measurement(hanbali_reading_tr_t1258, jurisprudential_method_kernel__hanbali_reading, theater_ratio, 1258, 0.2).
narrative_ontology:measurement(hanbali_reading_tr_t1328, jurisprudential_method_kernel__hanbali_reading, theater_ratio, 1328, 0.24).
narrative_ontology:measurement(hanbali_reading_tr_t1744, jurisprudential_method_kernel__hanbali_reading, theater_ratio, 1744, 0.33).
narrative_ontology:measurement(hanbali_reading_tr_t1932, jurisprudential_method_kernel__hanbali_reading, theater_ratio, 1932, 0.38).
narrative_ontology:measurement(hanbali_reading_tr_t2024, jurisprudential_method_kernel__hanbali_reading, theater_ratio, 2024, 0.45).

% Extraction over time
narrative_ontology:measurement(hanbali_reading_be_t855, jurisprudential_method_kernel__hanbali_reading, base_extractiveness, 855, 0.55).
narrative_ontology:measurement(hanbali_reading_be_t925, jurisprudential_method_kernel__hanbali_reading, base_extractiveness, 925, 0.58).
narrative_ontology:measurement(hanbali_reading_be_t1258, jurisprudential_method_kernel__hanbali_reading, base_extractiveness, 1258, 0.6).
narrative_ontology:measurement(hanbali_reading_be_t1328, jurisprudential_method_kernel__hanbali_reading, base_extractiveness, 1328, 0.62).
narrative_ontology:measurement(hanbali_reading_be_t1744, jurisprudential_method_kernel__hanbali_reading, base_extractiveness, 1744, 0.72).
narrative_ontology:measurement(hanbali_reading_be_t1932, jurisprudential_method_kernel__hanbali_reading, base_extractiveness, 1932, 0.76).
narrative_ontology:measurement(hanbali_reading_be_t2024, jurisprudential_method_kernel__hanbali_reading, base_extractiveness, 2024, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(hanbali_reading_su_t855, jurisprudential_method_kernel__hanbali_reading, suppression_requirement, 855, 0.35).
narrative_ontology:measurement(hanbali_reading_su_t925, jurisprudential_method_kernel__hanbali_reading, suppression_requirement, 925, 0.55).
narrative_ontology:measurement(hanbali_reading_su_t1258, jurisprudential_method_kernel__hanbali_reading, suppression_requirement, 1258, 0.48).
narrative_ontology:measurement(hanbali_reading_su_t1328, jurisprudential_method_kernel__hanbali_reading, suppression_requirement, 1328, 0.52).
narrative_ontology:measurement(hanbali_reading_su_t1744, jurisprudential_method_kernel__hanbali_reading, suppression_requirement, 1744, 0.7).
narrative_ontology:measurement(hanbali_reading_su_t1932, jurisprudential_method_kernel__hanbali_reading, suppression_requirement, 1932, 0.74).
narrative_ontology:measurement(hanbali_reading_su_t2024, jurisprudential_method_kernel__hanbali_reading, suppression_requirement, 2024, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jurisprudential_method_kernel__hanbali_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__hanbali_reading, jurisprudential_method_kernel__hanafi_reading).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__hanbali_reading, jurisprudential_method_kernel__maliki_reading).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__hanbali_reading, jurisprudential_method_kernel__shafii_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'Sunni legal methodology' decomposes into four structurally distinct readings of the jurisprudential_method_kernel, linked by network edges. This story (hanbali_reading) carries the family's highest epsilon because it uniquely condemns the shared instrument (reasoned extension) rather than ranking it (shafii), absorbing it into transmitted precedent (maliki), or licensing it (hanafi). The upstream story in empirical-confidence terms is the shafii_reading — its standardization is the family's reference settlement, and this reading's strict strand defines itself against it; the family should be analyzed as a unit, since each reading's beneficiary/victim structure is defined by what the others admit.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
