% ============================================================================
% CONSTRAINT STORY: common_law_precedent_corpus__evolutionary_framework
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_common_law_precedent_corpus__evolutionary_framework, []).

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
 *   constraint_id: common_law_precedent_corpus__evolutionary_framework
 *   human_readable: Common-Law Precedent as Adaptive Framework (Evolutionary Reading)
 *   domain: legal/jurisprudence/constitutional
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the common-law precedent kernel:
 *   precedent as an adaptive framework whose application is licensed to track
 *   contemporary normative evolution, with overruling normalized as
 *   corrective maintenance rather than exceptional rupture. The standing
 *   arrangement under contest — the ε referent — is the operating
 *   precedent-following practice as this reading understands it: courts
 *   extend, distinguish, and overrule prior decisions under a burden
 *   allocation that requires the status quo's defender, not the challenger,
 *   to justify persistence. The colloquial label 'precedent' decomposes into
 *   at least three structurally distinct constraints (this file plus the
 *   strict_stare_decisis and pluralist_balancing siblings); each carries its
 *   own epsilon, victim set, and classification, linked through
 *   network.affects_constraints. The claim/metric gap is deliberate: the
 *   arrangement is CLAIMED as tangled_rope (genuine coordination carrying
 *   asymmetric extraction under active enforcement) while the metrics
 *   describe moderately high, cyclically varying extraction with a rising
 *   enforcement-intensification trajectory — the engine measures the
 *   divergence; the claim is not tuned to predicted outputs.
 *
 * KEY AGENTS:
 *   - appellate_judiciary: agenda_setter and concentrated beneficiary (institutional/identity_locked) — authors the updates, collects the discretionary authority, cannot exit the office
 *   - reform_litigants: primary beneficiary (organized/mobile) — converts adverse precedent into challenge platforms through case selection
 *   - reliance_interest_holders: primary target (moderate/constrained) — absorb retroactive losses on completed arrangements
 *   - prior_winning_litigants: acute target (powerless/trapped) — victories converted to provisional status mid-dispute
 *   - lower_court_judges: intra-institutional target (institutional/constrained) — bear forecasting and reversal costs without agenda power
 *   - political_branches: excluded inter-institutional actor (powerful/constrained) — locked out of constitutional correction by amendment thresholds
 *   - general_public: diffuse beneficiary-payer (powerless/trapped) — receives adaptive correction, pays uncertainty, holds no procedural seat
 *   - legal_academia: analytical observer (institutional/analytical) — supplies the normative-evolution evidence base and audits its sincerity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(common_law_precedent_corpus__evolutionary_framework, 0.55).
domain_priors:suppression_score(common_law_precedent_corpus__evolutionary_framework, 0.53).
domain_priors:theater_ratio(common_law_precedent_corpus__evolutionary_framework, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(common_law_precedent_corpus__evolutionary_framework, extractiveness, 0.55).
narrative_ontology:constraint_metric(common_law_precedent_corpus__evolutionary_framework, suppression_requirement, 0.53).
narrative_ontology:constraint_metric(common_law_precedent_corpus__evolutionary_framework, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(common_law_precedent_corpus__evolutionary_framework, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(common_law_precedent_corpus__evolutionary_framework, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(common_law_precedent_corpus__evolutionary_framework, tangled_rope).
narrative_ontology:human_readable(common_law_precedent_corpus__evolutionary_framework, "Common-Law Precedent as Adaptive Framework (Evolutionary Reading)").
narrative_ontology:topic_domain(common_law_precedent_corpus__evolutionary_framework, "legal/jurisprudence/constitutional").

domain_priors:requires_active_enforcement(common_law_precedent_corpus__evolutionary_framework).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(common_law_precedent_corpus__evolutionary_framework, '2fe42fcc-53fe-45b4-b3de-44d42a34d829').
narrative_ontology:cs_kernel_codification('2fe42fcc-53fe-45b4-b3de-44d42a34d829', formalized).
narrative_ontology:cs_authority_grounding('2fe42fcc-53fe-45b4-b3de-44d42a34d829', expertise).
narrative_ontology:cs_interpretation_layer_present('2fe42fcc-53fe-45b4-b3de-44d42a34d829').
narrative_ontology:cs_reading_relation('2fe42fcc-53fe-45b4-b3de-44d42a34d829', common_law_precedent_corpus__strict_stare_decisis, coexists_with).
narrative_ontology:cs_reading_relation('2fe42fcc-53fe-45b4-b3de-44d42a34d829', common_law_precedent_corpus__pluralist_balancing, coexists_with).
narrative_ontology:cs_axiom('2fe42fcc-53fe-45b4-b3de-44d42a34d829', foundational, present_day_norms_constitute_law_content).
narrative_ontology:cs_axiom_status(present_day_norms_constitute_law_content, holdable).
narrative_ontology:cs_axiom_grounding('2fe42fcc-53fe-45b4-b3de-44d42a34d829', present_day_norms_constitute_law_content, deontological).
narrative_ontology:cs_axiom('2fe42fcc-53fe-45b4-b3de-44d42a34d829', secondary, overruling_is_corrective_maintenance).
narrative_ontology:cs_axiom_status(overruling_is_corrective_maintenance, holdable).
narrative_ontology:cs_axiom_grounding('2fe42fcc-53fe-45b4-b3de-44d42a34d829', overruling_is_corrective_maintenance, instrumental).
narrative_ontology:cs_reference_frame('2fe42fcc-53fe-45b4-b3de-44d42a34d829', adaptive_doctrinal_framework).
narrative_ontology:cs_drift_state('2fe42fcc-53fe-45b4-b3de-44d42a34d829', contemporary_originalist_resurgence, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('2fe42fcc-53fe-45b4-b3de-44d42a34d829', '').
narrative_ontology:cs_kernel_id(common_law_precedent_corpus__evolutionary_framework, common_law_precedent_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(common_law_precedent_corpus__evolutionary_framework, reform_litigants).
narrative_ontology:constraint_beneficiary(common_law_precedent_corpus__evolutionary_framework, appellate_judiciary).
narrative_ontology:constraint_beneficiary(common_law_precedent_corpus__evolutionary_framework, general_public).
narrative_ontology:constraint_victim(common_law_precedent_corpus__evolutionary_framework, reliance_interest_holders).
narrative_ontology:constraint_victim(common_law_precedent_corpus__evolutionary_framework, prior_winning_litigants).
narrative_ontology:constraint_victim(common_law_precedent_corpus__evolutionary_framework, lower_court_judges).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(common_law_precedent_corpus__evolutionary_framework, general_public).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sits atop the precedent hierarchy: decides which prior decisions to extend, distinguish, or overrule, and writes the opinions that recast doctrine in light of contemporary norms. Gains standing discretionary authority as the system's designated normative updater, and also absorbs the legitimacy costs whenever an update reads as result-driven. Exit is unavailable in any ordinary sense: the bench's professional identity is constituted by the interpretive office itself, and no external market exists for the role.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__evolutionary_framework, appellate_judiciary, agenda_setter,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(common_law_precedent_corpus__evolutionary_framework, appellate_judiciary, beneficiary).

% Public-interest organizations and repeat players who select test cases to press for doctrinal revision. Adverse precedent functions for them as a platform to argue from rather than a wall: they choose forums, plaintiffs, and timing, and a loss in one decade becomes the foundation for a renewed challenge in the next. Their mobility lies in case selection, not in escaping the legal system.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__evolutionary_framework, reform_litigants, beneficiary,
    organized, generational, mobile, national).

% Businesses, institutions, and individuals who ordered contracts, compliance programs, and life plans around settled doctrine. They cannot un-rely: when a court reinterprets, their completed arrangements absorb the loss retroactively, and their voice enters the process only after the rule has already shifted. Their practical options are insurance, lobbying for codification, or restructuring around whatever rule emerges next.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__evolutionary_framework, reliance_interest_holders, payer,
    moderate, biographical, constrained, national).

% Parties who prevailed under the prior rule and face its reversal in pending or reopened proceedings. They litigated under announced rules and won; the framework's corrective motion converts their victory into provisional status. Once the dispute is filed there is no exit — the same case that granted the win carries the risk of its revocation.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__evolutionary_framework, prior_winning_litigants, payer,
    powerless, immediate, trapped, national).

% Trial and intermediate appellate judges who must apply doctrine they anticipate will move. They carry reversal risk and the administrative cost of forecasting the direction of doctrinal travel, and hierarchy binds them to precedents the reviewing court may itself be preparing to abandon. Their institutional standing gives them voice in opinions but no vote on the destination.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__evolutionary_framework, lower_court_judges, payer,
    institutional, biographical, constrained, national).

% Legislatures and executives who can displace statutory interpretations by enactment but are locked out of constitutional correction by amendment supermajority requirements. They object that unelected judges are updating fundamental norms, yet possess no proportional corrective channel; their participation reduces to appointment politics, jurisdiction-curbing proposals, and occasional non-compliance threats.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__evolutionary_framework, political_branches, excluded,
    powerful, generational, constrained, national).

% Citizens subject to the resulting law. They receive the benefit of a legal order that can correct entrenched injustice without awaiting constitutional amendment, and they pay in reduced predictability and in the standing risk that protections they hold today may be re-read tomorrow. They hold no procedural seat; their input arrives aggregated and delayed through elections and appointments.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__evolutionary_framework, general_public, beneficiary,
    powerless, generational, trapped, national).
narrative_ontology:stakeholder_secondary_role(common_law_precedent_corpus__evolutionary_framework, general_public, payer).

% Law schools and scholarly networks that supply the 'contemporary normative evolution' evidentiary base — moral and political philosophy, social science, comparative law — which opinions cite, and that audit the gap between invoked norms and measured ones. They hold no decision power; their influence runs through clerkships, citation networks, and the credibility economy of the profession.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__evolutionary_framework, legal_academia, observer,
    institutional, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(common_law_precedent_corpus__evolutionary_framework, appellate_judiciary).
narrative_ontology:fixing_cost_class(common_law_precedent_corpus__evolutionary_framework, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Precedent-as-adaptive-framework coordinates legal decision-making across time and across courts: it supplies starting points, doctrinal structure, and shared reasoning methods so that like cases are treated alike without requiring every panel to rebuild the law from first principles, while licensing correction when accumulated decisions diverge from contemporary norms. It solves the problem of maintaining coherence without freezing error.
% TRANSFER_FUNCTION: Moves norm-setting discretion from historical enactment and settled decision to sitting appellate judges; moves predictability and reliance security away from those who arranged their affairs under prior doctrine toward those who challenge it; and, in each successful reinterpretation, transfers the concrete stake in a dispute from the prior rule's beneficiary to the current challenger.
% ABSENT_VOICES: Political branches are locked out of constitutional correction by amendment difficulty and would insist that fundamental norm-updating require broader consent. Reliance-interest holders have no seat at the moment reinterpretation is argued — they appear only after the rule has moved, holding completed arrangements. Citizens subject to updated norms have no participatory channel; their consent is mediated entirely through appointment politics.
% DISAPPEARANCE_RATIONALE: If the adaptive framework vanished overnight, the vacuum would fill with one of its rivals: under strict stare decisis the overruling pipeline closes, reliance security rises, and reform litigation loses its pathway; under unconstrained ad hoc judging, doctrinal coherence collapses and every dispute relitigates first principles. Either way the litigation economy, the rights-correction pathway, lower-court practice, and the judiciary's institutional self-concept all rearrange.
% FOUNDING_PROBLEM: The common law's founding problem: how to decide today's cases coherently given yesterday's decisions while avoiding the entrenchment of errors made under conditions that no longer obtain. The evolutionary reading was articulated to solve the specific failure mode of precedent rigidity outliving its justifying conditions.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians document recurring rigidity episodes in which entrenched doctrine persisted long after its justifying conditions lapsed, and comparative-law scholarship records the same stability-adaptation tension across jurisdictions. Notably, adherents of the sibling readings corroborate that the problem is live even while disputing the remedy — strict stare decisis concedes the rigidity risk and prices it as acceptable. Corroboration therefore exists from outside the benefiting parties; the benefiting parties' own attestation adds nothing probative.
narrative_ontology:disappearance_verdict(common_law_precedent_corpus__evolutionary_framework, world_rearranges).
narrative_ontology:founding_problem_status(common_law_precedent_corpus__evolutionary_framework, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(common_law_precedent_corpus__evolutionary_framework, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(common_law_precedent_corpus__evolutionary_framework, 'none', 1).
narrative_ontology:epsilon_provenance(common_law_precedent_corpus__evolutionary_framework, 0.55, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(common_law_precedent_corpus__evolutionary_framework_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(common_law_precedent_corpus__evolutionary_framework, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(common_law_precedent_corpus__evolutionary_framework_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.55 (interval end) because the arrangement continuously reprices settled expectations: every actor must discount announced rules by the probability of evolutionary revision, and that uncertainty is collected by no compensating party. Suppression (0.53) is a raw structural property, unscaled by power or scope: it reflects the enforced routing of norm change through judicial channels — litigants must frame challenges within the framework, losing parties cannot opt out of a reinterpretation, and the professional discipline polices which arguments count as legitimate evolution. Theater_ratio (0.42) captures the growing share of 'evolving standards' invocations that function as post-hoc rationalization of preferred outcomes rather than genuine norm-tracking; the series trends upward as the legitimacy vocabulary conventionalizes. Accessibility_collapse (0.40) is low-to-moderate: the framework legitimizes challenge rather than foreclosing it, though alternatives (codification, amendment) remain partly blocked. Resistance (0.55) is substantial and sustained — an organized originalist counter-jurisprudence, political-branch pushback, and academic critique. CYCLICAL PATTERN: the series shows roughly two full oscillations across the interval (activist surge peaking near t=16, backlash and retrenchment through t=24–32, reaffirmance churn near t=40, incremental trough near t=48, renewal from t=56 onward), driven by alternating court compositions and the backlash cycle each activist phase provokes. The oscillation is partly the extraction mechanism itself: intermittent, unpredictable reversal risk is what forces universal uncertainty discounting — a calm phase does not return collected uncertainty, it merely pauses collection. The suppression_requirement series is authored deliberately (rising trajectory) because the story specifically tracks enforcement-capacity change: the interpretive labor needed to police the boundary between legitimate reinterpretation and result-driven overruling, and to defend the framework's legitimacy, intensifies as contestation grows. IDENTITY-LOCK NOTE: the judiciary's exit_options are identity_locked through institutional identity fusion — the bench's self-concept is constituted by the interpretive office; if that frame broke (widespread acceptance that courts merely apply enacted text), the captured discretionary authority would evaporate and the operative constraint would migrate toward the strict sibling. All three tracked metrics run on one shared ten-point grid (t indexes years since 1948; t0=1948, tn=2020).
 *
 * PERSPECTIVAL GAP:
 *   Seat divergence is sharp and structural. From the reform-litigant seat the arrangement computes rope-like: exits are open, challenge is licensed, adverse precedent is raw material. From the reliance-holder and prior-winner seats the same structure operates snare-flavored: completed arrangements are repriced retroactively by a process they cannot enter until it concludes. From the appellate bench the framework is self-authored governance — the seat that writes the rule also experiences it as its own craft tradition. Lower-court judges occupy a distinctive same-level divergence from the appellate bench: nominally one institution, radically different seats — the apex holds agenda power on a generational horizon with identity-locked exit; the lower tiers bear the forecasting burden on a biographical horizon with hierarchical lock-in. Inter-institutionally, the political branches face the same constraint from outside the conversation entirely: powerful in every ordinary sense, yet excluded from the corrective channel by amendment thresholds, which is why their opposition routes into appointment politics. The engine computes these per-seat classifications from the structural data; the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations drive the derivation: reform_litigants (mobile exit, organized power) derive near the full-beneficiary end; reliance_interest_holders and prior_winning_litigants (constrained/trapped) derive near the full-target end; lower_court_judges derive as targets despite institutional power because hierarchy removes their exit. Two overrides are declared where the derivation would misread the seat. First, appellate_judiciary: the beneficiary declaration would derive a very low d (~0.1), but the seat also carries the constraint's legitimacy burden, is bound by the doctrine it authors, and cannot shed the office — d is overridden to 0.25, still beneficiary-side but materially above pure collector. Second, general_public: the beneficiary declaration would derive near-zero d, but diffuse uncertainty costs and total exclusion from the corrective channel place the seat nearer symmetric exposure — d is overridden to 0.45. Scope amplification applies modestly at national scale; the engine owns that arithmetic.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — precedent rigidity outliving its justifying conditions — remains live, attested by historians and by the sibling readings' own concessions, so no dead-mandate mismatch arises: status=live crossed with verdict=world_rearranges yields no zombie flag. The tangled_rope classification is what prevents mislabeling in both directions: reading the arrangement as pure coordination (rope) would erase the measurable retroactive losses borne by reliance seats; reading it as pure extraction (snare) would erase the demonstrated corrective function — documented overrulings that dismantled entrenched error. Piton decay is the live long-run risk and is monitored rather than asserted: a piton requires an atrophied function maintained theatrically, whereas here the corrective function demonstrably fires; but if the theater_ratio series crosses 0.5 — if 'contemporary norms' invocations become predominantly performative — the legitimacy vocabulary itself becomes the vestigial artifact, and the classification should be revisited. The rising suppression_requirement series is the early-warning indicator for that transition.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_delta,
    'This constraint is one reading (evolutionary_framework) of the common_law_precedent_corpus kernel; which structural facts would change under the sibling readings strict_stare_decisis and pluralist_balancing?',
    'Track which burden-allocation test dominates appellate overruling analysis across terms: the operative reading is identified by whether departure requires extraordinary justification, case-by-case domain weighing, or an ordinary contemporary-norms showing.',
    'Under strict_stare_decisis the victim set migrates to norm-challengers and epsilon falls sharply for reliance holders; under pluralist_balancing the unified epsilon here decomposes into domain-indexed values. Classification of every seat in this story is conditional on the reading remaining operative.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_delta, conceptual, 'Committer-frame omega: structural delta across sibling readings of the precedent kernel.').

omega_variable(
    norm_tracking_reference_class,
    'Does ''contemporary normative evolution'' as invoked by courts track broad public norms or professional and elite-class norms?',
    'Cross-reference the extrajudicial sources cited in updating opinions against mass survey and behavioral data contemporaneous with each decision.',
    'If tracking is elite-confined, the coordination story weakens and the arrangement extracts from popular sovereignty — pushing payer-seat classifications toward snare; if tracking is broad, the rope-side coordination function is confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(norm_tracking_reference_class, empirical, 'Whose norms does the framework actually track?').

omega_variable(
    invocation_sincerity_fraction,
    'What fraction of evolving-standards invocations are post-hoc rationalizations of preferred outcomes rather than genuine norm-tracking?',
    'Opinion-by-opinion coding of normative-evolution claims against contemporaneous social indicators, blinded to outcome, across the interval.',
    'A high sincerity-adjusted fraction would raise the true theater_ratio above the authored 0.42 and signal piton-drift of the legitimacy vocabulary; a low fraction confirms functional adaptation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(invocation_sincerity_fraction, empirical, 'Sincerity audit of the framework''s central justificatory language.').

omega_variable(
    reliance_loss_compensation,
    'Are reliance losses absorbed wholesale by affected parties, or systematically mitigated through transition rules, prospective-only overruling, and remedial tailoring?',
    'Doctrinal census of remedial treatment across overruling and major-reinterpretation cases in the interval.',
    'Uncompensated reliance loss raises effective extraction for the payer seats and supports snare-flavored per-seat classifications; systematic mitigation lowers it and stabilizes the tangled_rope reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reliance_loss_compensation, empirical, 'Whether the framework''s extraction carries compensating mechanisms.').

omega_variable(
    update_authority_allocation,
    'Should concentrated judicial authority over constitutional norm-updating be read as a defect to be corrected or a trusteeship to be preserved?',
    'Not resolvable by data alone: the verdict turns on prior commitments about counter-majoritarian difficulty, the relative reliability of courts versus legislatures as norm-trackers, and the value of rights insulation from electoral cycles.',
    'On the defect reading, the judiciary''s captured authority is extraction from the political branches and public; on the trusteeship reading, the same concentration is the price of the coordination function. The preference determines whether the agenda_setter seat''s gains are scored as rents or as compensation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(update_authority_allocation, preference, 'Value-level ambiguity in scoring the judiciary''s captured norm-updating authority.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(common_law_precedent_corpus__evolutionary_framework, 0, 72).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clpc_evo_fw_tr_t0, common_law_precedent_corpus__evolutionary_framework, theater_ratio, 0, 0.15).
narrative_ontology:measurement(clpc_evo_fw_tr_t8, common_law_precedent_corpus__evolutionary_framework, theater_ratio, 8, 0.18).
narrative_ontology:measurement(clpc_evo_fw_tr_t16, common_law_precedent_corpus__evolutionary_framework, theater_ratio, 16, 0.22).
narrative_ontology:measurement(clpc_evo_fw_tr_t24, common_law_precedent_corpus__evolutionary_framework, theater_ratio, 24, 0.27).
narrative_ontology:measurement(clpc_evo_fw_tr_t32, common_law_precedent_corpus__evolutionary_framework, theater_ratio, 32, 0.26).
narrative_ontology:measurement(clpc_evo_fw_tr_t40, common_law_precedent_corpus__evolutionary_framework, theater_ratio, 40, 0.31).
narrative_ontology:measurement(clpc_evo_fw_tr_t48, common_law_precedent_corpus__evolutionary_framework, theater_ratio, 48, 0.29).
narrative_ontology:measurement(clpc_evo_fw_tr_t56, common_law_precedent_corpus__evolutionary_framework, theater_ratio, 56, 0.33).
narrative_ontology:measurement(clpc_evo_fw_tr_t64, common_law_precedent_corpus__evolutionary_framework, theater_ratio, 64, 0.38).
narrative_ontology:measurement(clpc_evo_fw_tr_t72, common_law_precedent_corpus__evolutionary_framework, theater_ratio, 72, 0.42).

% Extraction over time
narrative_ontology:measurement(clpc_evo_fw_be_t0, common_law_precedent_corpus__evolutionary_framework, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(clpc_evo_fw_be_t8, common_law_precedent_corpus__evolutionary_framework, base_extractiveness, 8, 0.46).
narrative_ontology:measurement(clpc_evo_fw_be_t16, common_law_precedent_corpus__evolutionary_framework, base_extractiveness, 16, 0.54).
narrative_ontology:measurement(clpc_evo_fw_be_t24, common_law_precedent_corpus__evolutionary_framework, base_extractiveness, 24, 0.48).
narrative_ontology:measurement(clpc_evo_fw_be_t32, common_law_precedent_corpus__evolutionary_framework, base_extractiveness, 32, 0.4).
narrative_ontology:measurement(clpc_evo_fw_be_t40, common_law_precedent_corpus__evolutionary_framework, base_extractiveness, 40, 0.43).
narrative_ontology:measurement(clpc_evo_fw_be_t48, common_law_precedent_corpus__evolutionary_framework, base_extractiveness, 48, 0.38).
narrative_ontology:measurement(clpc_evo_fw_be_t56, common_law_precedent_corpus__evolutionary_framework, base_extractiveness, 56, 0.42).
narrative_ontology:measurement(clpc_evo_fw_be_t64, common_law_precedent_corpus__evolutionary_framework, base_extractiveness, 64, 0.5).
narrative_ontology:measurement(clpc_evo_fw_be_t72, common_law_precedent_corpus__evolutionary_framework, base_extractiveness, 72, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(clpc_evo_fw_su_t0, common_law_precedent_corpus__evolutionary_framework, suppression_requirement, 0, 0.22).
narrative_ontology:measurement(clpc_evo_fw_su_t8, common_law_precedent_corpus__evolutionary_framework, suppression_requirement, 8, 0.28).
narrative_ontology:measurement(clpc_evo_fw_su_t16, common_law_precedent_corpus__evolutionary_framework, suppression_requirement, 16, 0.34).
narrative_ontology:measurement(clpc_evo_fw_su_t24, common_law_precedent_corpus__evolutionary_framework, suppression_requirement, 24, 0.36).
narrative_ontology:measurement(clpc_evo_fw_su_t32, common_law_precedent_corpus__evolutionary_framework, suppression_requirement, 32, 0.33).
narrative_ontology:measurement(clpc_evo_fw_su_t40, common_law_precedent_corpus__evolutionary_framework, suppression_requirement, 40, 0.39).
narrative_ontology:measurement(clpc_evo_fw_su_t48, common_law_precedent_corpus__evolutionary_framework, suppression_requirement, 48, 0.37).
narrative_ontology:measurement(clpc_evo_fw_su_t56, common_law_precedent_corpus__evolutionary_framework, suppression_requirement, 56, 0.41).
narrative_ontology:measurement(clpc_evo_fw_su_t64, common_law_precedent_corpus__evolutionary_framework, suppression_requirement, 64, 0.47).
narrative_ontology:measurement(clpc_evo_fw_su_t72, common_law_precedent_corpus__evolutionary_framework, suppression_requirement, 72, 0.53).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(common_law_precedent_corpus__evolutionary_framework, enforcement_mechanism).
narrative_ontology:affects_constraint(common_law_precedent_corpus__evolutionary_framework, common_law_precedent_corpus__strict_stare_decisis).
narrative_ontology:affects_constraint(common_law_precedent_corpus__evolutionary_framework, common_law_precedent_corpus__pluralist_balancing).

% DUAL FORMULATION NOTE:
% The colloquial label 'precedent' decomposes into at least three structurally distinct constraints: this evolutionary_framework story, the strict_stare_decisis story, and the pluralist_balancing story. They assign different default burdens to departure and locate law-content differently, yielding different epsilon values, victim sets, and classifications — the decomposition follows the epsilon-invariance principle, since measuring 'precedent's force' through different observables yields different extraction profiles. The strict_stare_decisis story is upstream: its reliance-protection premises supply the baseline against which evolutionary departures must be justified, and its legitimacy capital is what this reading spends when it overrules. This story feeds the pluralist_balancing story downstream as a limiting case (uniform low burden approximates the evolutionary pole). Every family member links the others via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(common_law_precedent_corpus__evolutionary_framework, institutional, 0.25).
constraint_indexing:directionality_override(common_law_precedent_corpus__evolutionary_framework, powerless, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
