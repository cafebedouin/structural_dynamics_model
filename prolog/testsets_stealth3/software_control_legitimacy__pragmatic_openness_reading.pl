% ============================================================================
% CONSTRAINT STORY: software_control_legitimacy__pragmatic_openness_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_software_control_legitimacy__pragmatic_openness_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: software_control_legitimacy__pragmatic_openness_reading
 *   human_readable: Pragmatic Openness Norm — Software Control as Evidence-Arbitrated Methodology Choice
 *   domain: technological/political_economy_of_technology/intellectual_property
 *
 * SUMMARY:
 *   A normative arrangement governs how the software economy decides who
 *   controls code: the pragmatic openness frame, consolidated around the
 *   late-1990s 'open source' reframing, treats control as a
 *   development-methodology choice settled by evidence — open collaborative
 *   development where peer review measurably improves quality, proprietary
 *   closure where secrecy or speed dominates, both pre-legitimized. The frame
 *   runs on foundations' neutral stewardship, contributors' voluntary labor,
 *   and decision-makers' case-by-case judgment; it demands nothing of anyone
 *   and punishes no exit, which is why its extraction stays low while its
 *   reach is global. This story is ONE reading of the contested
 *   software-control kernel (pragmatic_openness_reading); the freedom,
 *   property-rights, and commons readings are separate files. Claim/metric
 *   independence: the rope claim is asserted from structure (genuine
 *   coordination, negligible coercion, net beneficiaries); the metric values
 *   are authored separately as descriptive estimates. KEY AGENTS (by
 *   structural relationship): - open_source_foundations: agenda setter
 *   (institutional/mobile) — stewards licenses and governance; authority
 *   rests on neutrality - volunteer_contributors: principal beneficiary with
 *   secondary payer position (moderate/mobile) — exchanges unpaid hours for
 *   review, skill, and standing - downstream_users: beneficiary
 *   (organized/constrained) — collects auditability and savings, absorbs
 *   integration and abandonment risk - hybrid_model_vendors: dual-positioned
 *   beneficiary (powerful/arbitrage) — monetizes both tracks -
 *   proprietary_software_vendors: tolerated beneficiary (powerful/arbitrage)
 *   — collects legitimacy, owes nothing - engineering_decision_makers:
 *   practitioner seat (moderate/constrained) — applies the heuristic daily -
 *   empirical_se_researchers: analytical observer (institutional/analytical)
 *   — supplies the evidence the frame runs on - freedom_imperative_advocates:
 *   excluded voice (moderate/identity_locked) — locked out by their own core
 *   premise - property_rights_advocates: excluded voice (powerful/arbitrage)
 *   — accepts tolerance, resents the asymmetry
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(software_control_legitimacy__pragmatic_openness_reading, 0.28).
domain_priors:suppression_score(software_control_legitimacy__pragmatic_openness_reading, 0.07).
domain_priors:theater_ratio(software_control_legitimacy__pragmatic_openness_reading, 0.34).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(software_control_legitimacy__pragmatic_openness_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(software_control_legitimacy__pragmatic_openness_reading, suppression_requirement, 0.07).
narrative_ontology:constraint_metric(software_control_legitimacy__pragmatic_openness_reading, theater_ratio, 0.34).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(software_control_legitimacy__pragmatic_openness_reading, accessibility_collapse, 0.22).
narrative_ontology:constraint_metric(software_control_legitimacy__pragmatic_openness_reading, resistance, 0.18).

% --- Constraint claim ---
narrative_ontology:constraint_claim(software_control_legitimacy__pragmatic_openness_reading, rope).
narrative_ontology:human_readable(software_control_legitimacy__pragmatic_openness_reading, "Pragmatic Openness Norm — Software Control as Evidence-Arbitrated Methodology Choice").
narrative_ontology:topic_domain(software_control_legitimacy__pragmatic_openness_reading, "technological/political_economy_of_technology/intellectual_property").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(software_control_legitimacy__pragmatic_openness_reading, 'e7fb9e1e-c3a6-4855-844d-4ace7af215a5').
narrative_ontology:cs_kernel_codification('e7fb9e1e-c3a6-4855-844d-4ace7af215a5', distributed).
narrative_ontology:cs_authority_grounding('e7fb9e1e-c3a6-4855-844d-4ace7af215a5', expertise).
narrative_ontology:cs_interpretation_layer_present('e7fb9e1e-c3a6-4855-844d-4ace7af215a5').
narrative_ontology:cs_reading_relation('e7fb9e1e-c3a6-4855-844d-4ace7af215a5', software_control_legitimacy__freedom_imperative_reading, forecloses).
narrative_ontology:cs_reading_relation('e7fb9e1e-c3a6-4855-844d-4ace7af215a5', software_control_legitimacy__property_rights_reading, coexists_with).
narrative_ontology:cs_reading_relation('e7fb9e1e-c3a6-4855-844d-4ace7af215a5', software_control_legitimacy__commons_reading, coexists_with).
narrative_ontology:cs_axiom('e7fb9e1e-c3a6-4855-844d-4ace7af215a5', foundational, methodology_selection_by_evidence).
narrative_ontology:cs_axiom_status(methodology_selection_by_evidence, holdable).
narrative_ontology:cs_axiom_grounding('e7fb9e1e-c3a6-4855-844d-4ace7af215a5', methodology_selection_by_evidence, instrumental).
narrative_ontology:cs_axiom('e7fb9e1e-c3a6-4855-844d-4ace7af215a5', foundational, proprietary_closure_is_legitimate).
narrative_ontology:cs_axiom_status(proprietary_closure_is_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('e7fb9e1e-c3a6-4855-844d-4ace7af215a5', proprietary_closure_is_legitimate, conventional).
narrative_ontology:cs_axiom('e7fb9e1e-c3a6-4855-844d-4ace7af215a5', secondary, open_collaboration_quality_superiority).
narrative_ontology:cs_axiom_status(open_collaboration_quality_superiority, holdable).
narrative_ontology:cs_axiom_grounding('e7fb9e1e-c3a6-4855-844d-4ace7af215a5', open_collaboration_quality_superiority, empirically_contingent).
narrative_ontology:cs_reference_frame('e7fb9e1e-c3a6-4855-844d-4ace7af215a5', evidence_arbitrated_methodology_pluralism).
narrative_ontology:cs_drift_state('e7fb9e1e-c3a6-4855-844d-4ace7af215a5', ai_weights_and_supply_chain_era, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('e7fb9e1e-c3a6-4855-844d-4ace7af215a5', '2026-08-11T09:42:00Z').
narrative_ontology:cs_kernel_id(software_control_legitimacy__pragmatic_openness_reading, software_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(software_control_legitimacy__pragmatic_openness_reading, volunteer_contributors).
narrative_ontology:constraint_beneficiary(software_control_legitimacy__pragmatic_openness_reading, downstream_users).
narrative_ontology:constraint_beneficiary(software_control_legitimacy__pragmatic_openness_reading, engineering_decision_makers).
narrative_ontology:constraint_beneficiary(software_control_legitimacy__pragmatic_openness_reading, hybrid_model_vendors).
narrative_ontology:constraint_beneficiary(software_control_legitimacy__pragmatic_openness_reading, proprietary_software_vendors).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(software_control_legitimacy__pragmatic_openness_reading, volunteer_contributors).
narrative_ontology:constraint_victim(software_control_legitimacy__pragmatic_openness_reading, hybrid_model_vendors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Host shared codebases, steward permissive licenses, and publish contribution-governance rules that treat commercial sponsors and hobbyists identically. Their standing depends on being seen as neutral ground: they neither reward opening code nor punish keeping it closed, they run the rails either choice travels on. If a foundation lost that neutrality it would bleed projects to rival hosts, so stewardship is continuously maintained and low-friction.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__pragmatic_openness_reading, open_source_foundations, agenda_setter,
    institutional, generational, mobile, global).

% Submit bug fixes and features to shared codebases outside working hours. They receive detailed review from strangers, sharpen skills employers pay for, accumulate public track records, and steer tools they personally depend on. The price is thousands of unpaid hours and periodic burnout when large corporate users take outputs without giving back. Nothing binds them: they can stop contributing or fork the project tomorrow at no penalty.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__pragmatic_openness_reading, volunteer_contributors, beneficiary,
    moderate, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(software_control_legitimacy__pragmatic_openness_reading, volunteer_contributors, payer).

% Build products and run infrastructure on components they did not write and may inspect freely. They save license fees, audit what they deploy, and get fixes quickly when maintainers are healthy; they absorb integration work, support gaps, and abandonment risk when maintainers are not. Moving any component to a proprietary supplier remains an ordinary procurement decision.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__pragmatic_openness_reading, downstream_users, beneficiary,
    organized, biographical, constrained, global).

% Publish a usable core under an open license and sell hardened, supported, or feature-complete editions. They recruit from the contributor pool, harvest ecosystem goodwill, and set de facto standards, while the open edition cannibalizes part of their addressable market and must be maintained. Relicensing or tightening the open/core boundary is always available and is exercised periodically.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__pragmatic_openness_reading, hybrid_model_vendors, beneficiary,
    powerful, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(software_control_legitimacy__pragmatic_openness_reading, hybrid_model_vendors, payer).

% Keep source closed and sell licenses or subscriptions. Under this arrangement their choice draws no condemnation and carries no penalty — it is named a legitimate alternative outright. They watch openness trends and selectively open pieces where recruiting, standard-setting, or ecosystem leverage pays, holding re-closure as a permanent live option.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__pragmatic_openness_reading, proprietary_software_vendors, beneficiary,
    powerful, biographical, arbitrage, global).

% Choose per project: open the code where peer review, interoperability, or talent attraction dominates the calculus; keep it closed where secrecy, shipping speed, or direct monetization dominates. They are the arrangement's working practitioners, translating the pluralist norm into thousands of small decisions, and they feel little pressure in either direction because both answers arrive pre-legitimized.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__pragmatic_openness_reading, engineering_decision_makers, beneficiary,
    moderate, biographical, constrained, national).

% Measure defect density, patch latency, and security outcomes across open and closed projects and publish comparisons. Their findings feed the arrangement's legitimacy, which is explicitly evidence-based; inconvenient null results are absorbed as boundary conditions — openness helps here, not there — rather than treated as attacks on the frame.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__pragmatic_openness_reading, empirical_se_researchers, observer,
    institutional, generational, analytical, global).

% Campaign on the position that control of one's own computing is a right, not a preference. They read the methodology framing as a surrender: whatever wins commercially becomes untouchable, and users inherit whatever restrictions the winner writes. They cannot adopt the pluralist frame without dissolving the ethical core they exist to defend, so they argue from permanently outside it — vocally, and with no procedural recourse inside its adjudication.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__pragmatic_openness_reading, freedom_imperative_advocates, excluded,
    moderate, civilizational, identity_locked, global).

% Industry intellectual-property councils and license hardliners who treat the right to restrict copying and modification as the creator's own entitlement. They welcome the frame's tolerance of closed models but chafe at its asymmetry: closure needs no justification here while openness carries a presumption of superiority they consider sentimental. From adjacent institutional positions they push stronger protective law — copy-protection mandates, anti-circumvention regimes, harsher enforcement.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__pragmatic_openness_reading, property_rights_advocates, excluded,
    powerful, generational, arbitrage, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(software_control_legitimacy__pragmatic_openness_reading, diffuse).
narrative_ontology:fixing_cost_class(software_control_legitimacy__pragmatic_openness_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allocates scarce engineering attention and trust across a global software economy: it routes work toward collaboratively reviewed codebases where distributed scrutiny measurably raises quality, while releasing firms and projects from any obligation to open code where secrecy or speed dominates — solving the when-to-share problem case by case instead of by doctrine.
% TRANSFER_FUNCTION: Moves paid and unpaid engineering labor into shared repositories; moves review attention from peers to submitters; moves reputational credit and hiring signal to visible contributors; moves quality, auditability, and fee savings to users; and moves decision authority over disclosure from ideological camps to project-level cost-benefit judgment.
% ABSENT_VOICES: Freedom-imperative advocates would insist user control is an end in itself and that recasting control as methodology launders proprietary domination as mere style. Property-rights hardliners would insist creator restriction rights need no utilitarian defense and resent the frame's built-in presumption favoring openness. Both are loud in general public discourse but out-of-frame here: the reading classifies their objections as value preferences lying outside its evidence-based remit.
% DISAPPEARANCE_RATIONALE: Corporate open-source program offices, dual-licensing strategies, procurement scorecards, and hiring pipelines all reference this frame. Overnight removal would push methodology decisions back into the ideological camps it was built to bypass — freedom-versus-property fights would re-absorb day-to-day tooling choices, and the neutral 'best tool for the job' vocabulary that lets commercial rivals collaborate would thin out. Existing codebases, licenses, and foundations would persist; the arbitration layer above them would reorganize.
% FOUNDING_PROBLEM: In the late 1990s collaborative free software carried an ideological charge that made corporations treat participation as a liability. The pragmatic reframing was built to solve that adoption problem: give businesses a non-ideological vocabulary ('open source') and an evidence-based rationale (quality through peer review) under which joining shared development became a defensible management decision rather than a political statement.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the current beneficiary set by contemporaneous 1998 press coverage quoting executives explaining the rename from 'free software' to 'open source' as investor-relations strategy; by historians of computing who document the deliberate de-politicization around the Open Source Initiative's founding; and — most strongly — by the freedom-movement faction that opposed the renaming at the time precisely for trading ethics for respectability. Adversaries attesting your motive is strong corroboration. No serious participant disputes that adoption-palatability was the founding motive; what remains disputed is whether the trade was worth making.
narrative_ontology:disappearance_verdict(software_control_legitimacy__pragmatic_openness_reading, world_rearranges).
narrative_ontology:founding_problem_status(software_control_legitimacy__pragmatic_openness_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(software_control_legitimacy__pragmatic_openness_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(software_control_legitimacy__pragmatic_openness_reading, 'none', 1).
narrative_ontology:epsilon_provenance(software_control_legitimacy__pragmatic_openness_reading, 0.28, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(software_control_legitimacy__pragmatic_openness_reading_tests).
:- end_tests(software_control_legitimacy__pragmatic_openness_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Metrics authored descriptively under this reading's own lights, independently of the rope claim. Extractiveness 0.28: participation is voluntary and reciprocated, but the arrangement leaks — corporate users take maintainer output without reciprocal contribution, open-core firms convert volunteer labor into upsell revenue, and open-washing converts the norm's prestige into marketing cover — a slow creep from the 0.10 baseline at the 1998 founding. Suppression 0.07: the frame coerces essentially no one; keeping code closed is expressly legitimate, so there is nothing to enforce and no exit to punish. Theater_ratio 0.34: peer review and collaboration are real and dominant, but checkbox adoption, badge collection, and open-washing have grown steadily since corporate adoption peaked. Accessibility_collapse 0.22: alternatives do not collapse — proprietary, hybrid, and closed paths remain fully choosable, which is the frame's defining structural feature. Resistance 0.18: ideological camps resist the framing itself, but a constraint that demands so little attracts only ceremonial resistance. No suppression_requirement series is authored: enforcement capacity is static and near-zero across the interval, so the story-level scalar carries the picture. Gain_flow 'diffuse' is an affirmative check, not a default: each seated party was examined for capture of the arrangement's extraction and none captures it — costs recycle as reputation, skill, and software quality; the nearest candidate (hybrid vendors converting volunteer labor to revenue) nets against the open edition they fund. Fixing or removing the norm is cheap: it runs on discourse and habit, not machinery.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. Foundations experience neutral service stewardship; contributors experience a favorable exchange they can leave at will; vendors experience costless optionality; decision-makers experience convenience; excluded ideologues experience erasure — the same arrangement that looks like generous neutrality from inside looks like a rigged vocabulary game from the freedom-imperative seat, because the frame settles by fiat the very question (is closure legitimate?) that seat treats as the entire contest. The engine computes this divergence from the structural data; the authored rope claim adjudicates nothing.
 *
 * DIRECTIONALITY LOGIC:
 *   Every seated party declares net benefit, so the structural derivation places the beneficiary seats near the beneficiary pole (low d), and engine-side effective extraction stays small for each even at global scope — scope scales extractiveness modestly upward, but base epsilon is low enough that amplified chi remains small. volunteer_contributors carry a secondary payer position: their uncompensated hours pull their derived d somewhat toward symmetric, and their seat is where any drift toward tangling would register first. The two excluded voices derive no beneficiary or victim stake — they sit outside the transfer paths by construction; their exclusion IS their structural position, and per the R3 ruling it feeds the consensus-provenance check, never a classification override. Suppression enters the computation unscaled as the raw structural 0.07. No directionality overrides are used: the derivation chain produces accurate d values from the declarations alone.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — making collaborative development acceptable to business — is historically dead: open source won, and the frame's own architects describe the original mission as superseded. Yet the arrangements persist and perform: methodology arbitration migrated to new frontiers (cloud service openness, AI weight disclosure) where the 'which model serves quality here?' question recurs. This story therefore authors the R5 mismatch (dead founding problem x world_rearranges) deliberately; it should resolve as renewal, not zombification, because theater_ratio stays below 0.5, gains remain diffuse, and fixing is cheap — the transient-renewal side of the receipt grid, not the piton cell. The residual risk is tracked in the successor_function_durability omega: if the arbitration function goes quiet while the vocabulary persists, the frame decays toward theatrical maintenance and the classification would follow the data.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'Is the low epsilon measured here a property of the pragmatic reading''s referent alone, or does the standing arrangement measure differently under the sibling readings?',
    'Compare compiled chi and classification across the four sibling files over identical structural inputs; divergence is expected and diagnostic.',
    'Confirms epsilon is reading-indexed over a fixed referent: divergent sibling verdicts are the corpus''s intended indexical measurement, not an inconsistency to reconcile.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Reading-indexed epsilon over the shared software-control referent').

omega_variable(
    quality_superiority_empirical_status,
    'Does the frame''s load-bearing empirical claim — that open collaboration yields superior quality and security — survive current evidence (supply-chain compromises such as Heartbleed, Log4Shell, and the xz backdoor; maintainer burnout)?',
    'Systematic meta-analysis of defect, velocity, and security outcomes across open and closed projects, controlling for project age, domain, and funding.',
    'Sustained falsification would convert the instrumental axiom toward overridden status, deepen the axiom_overriding drift vector, and potentially demote the frame to a theatrical justification for cost-free corporate participation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(quality_superiority_empirical_status, empirical, 'Empirical standing of the peer-review quality advantage claim').

omega_variable(
    successor_function_durability,
    'With the founding problem dead, is the ongoing methodology-arbitration function durable, or a transitional residue awaiting a new settlement as the AI-weights openness contest reopens the question?',
    'Track whether the evidence-arbitration vocabulary survives the AI-weight disclosure contest or collapses back into revived freedom-versus-property camps.',
    'If the vocabulary collapses while the function goes quiet, the constraint decays toward theatrical maintenance; if the arbitration function holds, the low-extraction coordination reading stabilizes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(successor_function_durability, empirical, 'Durability of the post-adoption arbitration function').

omega_variable(
    contributor_reciprocity_balance,
    'Is the contributors'' net position actually positive as the reading claims, or drifting negative under corporate free-riding, unpaid maintenance burdens, and burnout?',
    'Longitudinal contributor retention surveys and compensation-flow studies tracing where maintenance labor originates and where value lands.',
    'A sustained net-negative position would authorize a victim set the reading currently declines to name, pulling the structural classification toward a coordination-plus-extraction hybrid and raising effective chi for the corporate-user seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(contributor_reciprocity_balance, empirical, 'Whether the no-victim structural delta remains honest').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(software_control_legitimacy__pragmatic_openness_reading, 1998, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(soft_tr_t1998, software_control_legitimacy__pragmatic_openness_reading, theater_ratio, 1998, 0.1).
narrative_ontology:measurement_basis(soft_tr_t1998, observed).
narrative_ontology:measurement(soft_tr_t2003, software_control_legitimacy__pragmatic_openness_reading, theater_ratio, 2003, 0.14).
narrative_ontology:measurement_basis(soft_tr_t2003, observed).
narrative_ontology:measurement(soft_tr_t2008, software_control_legitimacy__pragmatic_openness_reading, theater_ratio, 2008, 0.18).
narrative_ontology:measurement_basis(soft_tr_t2008, observed).
narrative_ontology:measurement(soft_tr_t2013, software_control_legitimacy__pragmatic_openness_reading, theater_ratio, 2013, 0.22).
narrative_ontology:measurement_basis(soft_tr_t2013, observed).
narrative_ontology:measurement(soft_tr_t2018, software_control_legitimacy__pragmatic_openness_reading, theater_ratio, 2018, 0.26).
narrative_ontology:measurement_basis(soft_tr_t2018, observed).
narrative_ontology:measurement(soft_tr_t2021, software_control_legitimacy__pragmatic_openness_reading, theater_ratio, 2021, 0.29).
narrative_ontology:measurement_basis(soft_tr_t2021, observed).
narrative_ontology:measurement(soft_tr_t2024, software_control_legitimacy__pragmatic_openness_reading, theater_ratio, 2024, 0.32).
narrative_ontology:measurement_basis(soft_tr_t2024, observed).
narrative_ontology:measurement(soft_tr_t2026, software_control_legitimacy__pragmatic_openness_reading, theater_ratio, 2026, 0.34).
narrative_ontology:measurement_basis(soft_tr_t2026, projected).

% Extraction over time
narrative_ontology:measurement(soft_be_t1998, software_control_legitimacy__pragmatic_openness_reading, base_extractiveness, 1998, 0.1).
narrative_ontology:measurement_basis(soft_be_t1998, observed).
narrative_ontology:measurement(soft_be_t2003, software_control_legitimacy__pragmatic_openness_reading, base_extractiveness, 2003, 0.13).
narrative_ontology:measurement_basis(soft_be_t2003, observed).
narrative_ontology:measurement(soft_be_t2008, software_control_legitimacy__pragmatic_openness_reading, base_extractiveness, 2008, 0.16).
narrative_ontology:measurement_basis(soft_be_t2008, observed).
narrative_ontology:measurement(soft_be_t2013, software_control_legitimacy__pragmatic_openness_reading, base_extractiveness, 2013, 0.19).
narrative_ontology:measurement_basis(soft_be_t2013, observed).
narrative_ontology:measurement(soft_be_t2018, software_control_legitimacy__pragmatic_openness_reading, base_extractiveness, 2018, 0.22).
narrative_ontology:measurement_basis(soft_be_t2018, observed).
narrative_ontology:measurement(soft_be_t2021, software_control_legitimacy__pragmatic_openness_reading, base_extractiveness, 2021, 0.24).
narrative_ontology:measurement_basis(soft_be_t2021, observed).
narrative_ontology:measurement(soft_be_t2024, software_control_legitimacy__pragmatic_openness_reading, base_extractiveness, 2024, 0.26).
narrative_ontology:measurement_basis(soft_be_t2024, observed).
narrative_ontology:measurement(soft_be_t2026, software_control_legitimacy__pragmatic_openness_reading, base_extractiveness, 2026, 0.28).
narrative_ontology:measurement_basis(soft_be_t2026, projected).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(software_control_legitimacy__pragmatic_openness_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(software_control_legitimacy__pragmatic_openness_reading, resource_allocation).
narrative_ontology:affects_constraint(software_control_legitimacy__pragmatic_openness_reading, freedom_imperative_reading).
narrative_ontology:affects_constraint(software_control_legitimacy__pragmatic_openness_reading, property_rights_reading).
narrative_ontology:affects_constraint(software_control_legitimacy__pragmatic_openness_reading, commons_reading).

% DUAL FORMULATION NOTE:
% Kernel decomposition note: 'software control legitimacy' is one colloquial label covering four structurally distinct commitments. This file is the pragmatic_openness_reading — low extraction, no victim set, beneficiaries across the developer, user, and vendor spectrum. The freedom_imperative_reading authors the same referent with a proprietary-victim structure and categorical illegitimacy; the property_rights_reading authors restriction as creator entitlement; the commons_reading authors negotiated collective management with its own asymmetries. Per the epsilon-invariance principle these are separate stories linked here rather than one story with a measurement parameter; divergence in their computed classifications over the same referent is the family's purpose, not a defect.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
