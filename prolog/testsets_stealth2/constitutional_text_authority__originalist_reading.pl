% ============================================================================
% CONSTRAINT STORY: constitutional_text_authority__originalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_text_authority__originalist_reading, []).

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
 *   constraint_id: constitutional_text_authority__originalist_reading
 *   human_readable: Originalist Fixity Gate on Constitutional Meaning
 *   domain: legal/constitutional/jurisprudential
 *
 * SUMMARY:
 *   The standing arrangement under contest is the originalist interpretive
 *   regime: constitutional outcomes are gated on the historical public
 *   understanding of the enacted text at its ratification, and
 *   post-ratification social change alters constitutional meaning only
 *   through Article V amendment. The regime performs a real coordination
 *   function — it gives thousands of judges a shared decision criterion,
 *   makes constitutional law traceable to enacted text rather than sitting
 *   judges' values, and reserves semantic change to the supermajoritarian
 *   amendment process. The same structure imposes asymmetric costs: litigants
 *   asserting rights grounded in post-ratification moral insight find the
 *   courthouse door gated by evidence they cannot supply, groups whose equal
 *   standing was absent from the ratification-era polity must litigate their
 *   way into a historical record that did not include them, and judges who
 *   would reason from contemporary principle have those argument routes ruled
 *   out of order. Enforcement is active and intensifying: confirmation
 *   screening, doctrinal supervision through reversals, and a professional
 *   apparatus that produces the historical evidence on which permissible
 *   outcomes depend. This file instantiates ONE reading of the
 *   constitutional_text_authority kernel; the sibling readings are separate
 *   constraint files linked through the network section, and the committer
 *   structure is carried in the omega variables rather than folded into this
 *   classification.
 *
 * KEY AGENTS:
 *   - originalist_supreme_court_justices: Agenda setter (institutional/identity_locked) — decides which historical evidence counts, writes the gating opinions, disciplines lower courts; life tenure fuses professional identity with the method
 *   - adaptive_interpretation_judges: Payer (institutional/identity_locked) — judges who would ground decisions in contemporary moral principle; their argument routes are foreclosed and they bear reversal and dissent costs
 *   - unenumerated_rights_claimants: Payer (moderate/trapped) — litigants asserting rights grounded in post-ratification moral insight; no alternative forum once the gate closes
 *   - post_ratification_inclusion_seekers: Payer (organized/trapped) — groups whose equal standing emerged after the ratification-era understanding; must argue their inclusion from a record that excluded them
 *   - enumerated_rights_holders: Beneficiary (moderate/mobile) — citizens asserting textually explicit rights receive robust, stable protection under the gate
 *   - status_quo_settlement_coalitions: Beneficiary (powerful/arbitrage) — holders of existing constitutional settlements whose positions are frozen behind the amendment supermajority
 *   - originalist_legal_academy: Beneficiary with secondary agenda-setting function (moderate/identity_locked) — careers and research infrastructure built on producing the gating historical evidence
 *   - article_v_state_legislatures: Excluded (institutional/constrained) — hold the formal amendment power but sit outside the day-to-day constitutional conversation, which happens in courtrooms
 *   - jurisprudential_analysts: Observer (analytical/analytical) — methodologically diverse legal theorists who see the full structure and attest to or dispute its operation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_text_authority__originalist_reading, 0.6).
domain_priors:suppression_score(constitutional_text_authority__originalist_reading, 0.62).
domain_priors:theater_ratio(constitutional_text_authority__originalist_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_text_authority__originalist_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(constitutional_text_authority__originalist_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(constitutional_text_authority__originalist_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_text_authority__originalist_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(constitutional_text_authority__originalist_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_text_authority__originalist_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_text_authority__originalist_reading, "Originalist Fixity Gate on Constitutional Meaning").
narrative_ontology:topic_domain(constitutional_text_authority__originalist_reading, "legal/constitutional/jurisprudential").

domain_priors:requires_active_enforcement(constitutional_text_authority__originalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_text_authority__originalist_reading, '1ad0103a-a722-4d65-a339-a6889b15ed74').
narrative_ontology:cs_kernel_codification('1ad0103a-a722-4d65-a339-a6889b15ed74', fixed_text).
narrative_ontology:cs_authority_grounding('1ad0103a-a722-4d65-a339-a6889b15ed74', lineage).
narrative_ontology:cs_interpretation_layer_present('1ad0103a-a722-4d65-a339-a6889b15ed74').
narrative_ontology:cs_reading_relation('1ad0103a-a722-4d65-a339-a6889b15ed74', constitutional_text_authority__living_constitutionalist_reading, forecloses).
narrative_ontology:cs_reading_relation('1ad0103a-a722-4d65-a339-a6889b15ed74', constitutional_text_authority__positivist_reading, influences).
narrative_ontology:cs_axiom('1ad0103a-a722-4d65-a339-a6889b15ed74', foundational, semantic_fixity_at_ratification).
narrative_ontology:cs_axiom_status(semantic_fixity_at_ratification, holdable).
narrative_ontology:cs_axiom_grounding('1ad0103a-a722-4d65-a339-a6889b15ed74', semantic_fixity_at_ratification, deontological).
narrative_ontology:cs_axiom('1ad0103a-a722-4d65-a339-a6889b15ed74', secondary, amendment_exclusive_semantic_change).
narrative_ontology:cs_axiom_status(amendment_exclusive_semantic_change, holdable).
narrative_ontology:cs_axiom_grounding('1ad0103a-a722-4d65-a339-a6889b15ed74', amendment_exclusive_semantic_change, conventional).
narrative_ontology:cs_reference_frame('1ad0103a-a722-4d65-a339-a6889b15ed74', ratified_public_understanding_baseline).
narrative_ontology:cs_drift_state('1ad0103a-a722-4d65-a339-a6889b15ed74', contemporary_history_and_tradition_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('1ad0103a-a722-4d65-a339-a6889b15ed74', '').
narrative_ontology:cs_kernel_id(constitutional_text_authority__originalist_reading, constitutional_text_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_text_authority__originalist_reading, enumerated_rights_holders).
narrative_ontology:constraint_beneficiary(constitutional_text_authority__originalist_reading, status_quo_settlement_coalitions).
narrative_ontology:constraint_beneficiary(constitutional_text_authority__originalist_reading, originalist_legal_academy).
narrative_ontology:constraint_victim(constitutional_text_authority__originalist_reading, unenumerated_rights_claimants).
narrative_ontology:constraint_victim(constitutional_text_authority__originalist_reading, post_ratification_inclusion_seekers).
narrative_ontology:constraint_victim(constitutional_text_authority__originalist_reading, adaptive_interpretation_judges).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Life-tenured justices who decide which historical evidence counts as the public understanding that gates outcomes, write the majority opinions that discipline lower courts through reversal, and screen successors through the confirmation process. Their exit is retirement only; their jurisprudential identity, opinions, and legacies are built on the method, so abandoning it would repudiate their own body of work.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__originalist_reading, originalist_supreme_court_justices, agenda_setter,
    institutional, generational, identity_locked, national).

% Judges and justices who would ground constitutional decisions in contemporary moral principle or evolving national values. Under the gate, those argument routes are ruled out of order: they write dissents and separate opinions, suffer reversal on lower benches for methodological deviation, and cannot resign into a judiciary that welcomes their method. Their professional formation and ambitions are invested in interpretive practices the constraint forbids.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__originalist_reading, adaptive_interpretation_judges, payer,
    institutional, generational, identity_locked, national).

% Litigants asserting rights grounded in post-ratification moral insight — dignity claims, emerging privacy interests, novel equality arguments. The gate requires them to show the historical record already contained their right; where it does not, their claim fails regardless of its contemporary force. Once the supreme court closes the route, no alternative forum exists; the nominal exit, Article V amendment, costs more than any individual litigant or single-case coalition can mount.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__originalist_reading, unenumerated_rights_claimants, payer,
    moderate, biographical, trapped, national).

% Groups whose equal standing crystallized after the ratification-era public understanding was formed — communities excluded from the founding and reconstruction polities whose descendants now claim constitutional protection. They must argue their inclusion from a historical record that did not include them, through advocacy organizations that can fund decades of litigation but cannot conjure ratification-era evidence. Their generational projects depend on a gate keyed to a polity that rejected their membership.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__originalist_reading, post_ratification_inclusion_seekers, payer,
    organized, generational, trapped, national).

% Citizens asserting rights the text states explicitly — speech, religious exercise, due process, jury trial. The gate gives their claims durable, predictable protection insulated from judicial mood: a right anchored in 1791 or 1868 understanding does not erode with doctrinal fashion. They carry little of the constraint's cost and can relocate, speak, and organize freely within its protection.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__originalist_reading, enumerated_rights_holders, beneficiary,
    moderate, biographical, mobile, national).

% Political and economic coalitions holding existing constitutional settlements — established property arrangements, entrenched regulatory boundaries, settled rights doctrines. The amendment supermajority freezes their positions against ordinary political majorities, and they possess the resources to fund friendly historical scholarship, select favorable forums, and wait out challengers. Every year the gate holds is a year their settlement compounds.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__originalist_reading, status_quo_settlement_coalitions, beneficiary,
    powerful, generational, arbitrage, national).

% Scholars, research centers, and corpus-linguistics projects that produce the founding-era evidence on which permissible outcomes turn. Careers, journals, and databases are built on the method's demand for historical input; they materially shape which evidence exists and how it is framed, giving them a secondary agenda-setting hand inside the gate. Leaving the method means abandoning the professional identity and infrastructure their work constitutes.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__originalist_reading, originalist_legal_academy, beneficiary,
    moderate, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(constitutional_text_authority__originalist_reading, originalist_legal_academy, agenda_setter).

% State legislatures hold the formal power to propose and ratify constitutional amendments — the constraint's designated change channel — yet the day-to-day constitutional conversation happens in courtrooms they do not occupy. They would object from both directions: that courts are deciding what their process should decide, and that the gate's rigidity makes their process unusable for the changes their constituents demand. They are structurally present in the design and absent from its operation.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__originalist_reading, article_v_state_legislatures, excluded,
    institutional, generational, constrained, national).

% Legal theorists, historians, and comparative constitutional scholars of diverse methodological commitments who study the regime from outside its enforcement. They attest to what the historical record does and does not support, measure whether the method constrains outcomes, and compare amendment costs across systems. They neither collect from the gate nor pay into it, which is what makes their corroboration usable as external evidence about the founding problem.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__originalist_reading, jurisprudential_analysts, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(constitutional_text_authority__originalist_reading, status_quo_settlement_coalitions).
narrative_ontology:fixing_cost_class(constitutional_text_authority__originalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the judicial-discretion and constitutional-indeterminacy problem: fixes a shared, publicly ascertainable criterion (historical public understanding of enacted text) so that thousands of judges reach traceable outcomes, constitutional law remains connected to the document that was actually ratified, and semantic change is reserved to the supermajoritarian amendment process rather than decided case-by-case by sitting courts.
% TRANSFER_FUNCTION: Moves interpretive authority from sitting judges to the ratification-era public (operating through its modern surrogates — historians, corpus linguists, the legal academy); moves the cost of constitutional change from courts, where it is cheap and case-by-case, to amendment campaigns, where it is prohibitively expensive; and moves security of existing settlements to whoever currently holds them.
% ABSENT_VOICES: Those excluded from the ratification-era polity whose understanding fixes the meaning — enslaved people, women, the unpropertied at 1791, and their counterparts at 1868 — together with present-day claimants whose moral insights postdate every ratification. They appear in the regime only as objects of historical inquiry, not as speakers: the gate consults the dead polity's understanding and has no procedural slot for the living excluded to contest what counts.
% DISAPPEARANCE_RATIONALE: If the gate vanished overnight, constitutional adjudication would reorganize around whatever criterion replaced it — moral principle, prudence, precedent-plus — reopening unenumerated-rights dockets that the gate closed, stripping settlement holders of the assurance their positions currently enjoy, collapsing the demand side of the founding-era evidence industry, and shifting change-control back toward courts and away from the amendment process. Every named seat's situation changes; the arrangement is load-bearing.
% FOUNDING_PROBLEM: Twentieth-century expansion of judicial discretion under broad moral readings of the Constitution raised the problem of unelected judges deciding contested political-moral questions under cover of interpretation, decoupling constitutional outcomes from the enacted text and from the democratic process that amend it.
% FOUNDING_PROBLEM_CORROBORATION: Methodologically diverse theorists outside the beneficiary set corroborate that the discretion problem is real — living-constitutionalist and pragmatist jurists propose their own constraints on judicial discretion, and comparative constitutional scholarship treats the countermajoritarian difficulty as a genuine structural problem — while disputing whether historical fixation solves it rather than relocating discretion into evidence selection. No corroborating source attests that the problem is dead; the dispute over the remedy is live in the academic and judicial record.
narrative_ontology:disappearance_verdict(constitutional_text_authority__originalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_text_authority__originalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_text_authority__originalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(constitutional_text_authority__originalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_text_authority__originalist_reading, 0.6, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_text_authority__originalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(constitutional_text_authority__originalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(constitutional_text_authority__originalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored at 0.60 because the gate's costs fall asymmetrically: the same structure that protects enumerated rights and stabilizes settlements prices change-seekers out of constitutional recognition entirely, and the Article V alternative is prohibitively expensive relative to case-by-case adjudication. Suppression is 0.62 and reflects a structural mechanism — doctrinal exclusion of non-historical argument routes, reversal discipline on lower courts, confirmation screening — with a smaller internalized component among identity-fused judges and scholars (roughly 70% structural, 30% internalized; see the identity-lock omega). Suppression is authored as a raw structural property and is not scaled by power or scope; only extractiveness is scaled, by the engine, from directionality and scope. Theater ratio is 0.42: the historical enterprise is partly genuine method and partly rhetorical performance — selective dictionaries, curated corpora, law-office history deployed to launder predetermined conclusions — and the rising series tracks the growth of that performative layer as the method matured from academic position to governing doctrine. Accessibility collapse is 0.45 because alternatives do not vanish: rival methodologies persist in scholarship and dissents, state constitutions and legislation remain open channels, and amendment remains formally available — but within an originalist-controlled court the argumentative alternatives collapse for the litigant standing before it. Resistance is 0.70: sustained academic counter-literature, contested confirmations, and dissenting opinions constitute real, ongoing resistance from those the gate excludes. The measurement series run on one single shared time grid (points 0-50, mapping approximately 1975-2025) with all three metrics authored at every point; trajectories are monotonic rather than cyclical, reflecting accumulation rather than oscillation — there is no intermittent-reinforcement mechanism here, only ratcheting enforcement and maturing doctrine. Coalition note: the payer seats are not without recourse — claimants, inclusion seekers, and movements can in principle coalition through Article V campaigns, and the constraint's design deliberately channels them there; the question the amendment-threshold omega addresses is whether that channel is a genuine exit or a priced-out one.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute divergent types from identical structural data. From the agenda-setter seat (committed justices), the arrangement is legitimate self-government through law: a fixed criterion they faithfully apply, with extraction invisible because the costs fall on people who 'should have amended.' From the payer seats (claimants, inclusion seekers, adaptive judges), the same structure is a gate that converts their moral claims into historical research problems they usually lose, enforced by people whose careers depend on the gate's authority. From the beneficiary seats, it is insurance: enumerated-rights holders receive durable protection and settlement holders receive frozen assurances. The engine computes these per-seat classifications from the declared roles, power atoms, and exit options; the authored claim does not adjudicate between them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries derive low directionality: enumerated_rights_holders are subsidized by the gate (their claims pass; others' do not), status_quo_settlement_coalitions sit nearest the full-beneficiary end (arbitrage-grade exit — they can fund litigation, shop forums, and wait out opponents), and the originalist_legal_academy collects professional rents from producing the gating evidence. Victims derive high directionality, amplified by exit structure: unenumerated_rights_claimants and post_ratification_inclusion_seekers are trapped (Article V is the nominal exit but its cost is the extraction mechanism itself), and adaptive_interpretation_judges are identity_locked — their professional selves are invested in argumentative practices the gate forbids. The agenda-setter justices are not declared beneficiaries; their relationship is administrative, and the engine's fallback for their power atom applies. No directionality overrides were needed: the beneficiary/victim declarations plus exit options capture the structural relationships, and overriding per power atom would misfire since multiple distinct seats share atoms.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — unelected judges deciding contested moral questions under cover of interpretation — is assessed as contested rather than dead: methodologically diverse theorists outside the beneficiary set corroborate that the discretion problem is real while disputing whether historical fixation solves it. Because the mandate is not resolved, this is not yet a mandatrophy case; the classification's job is to prevent the two symmetric mislabels. Reading it as pure snare erases the genuine coordination function (shared criterion, democratic change-routing, settlement stability) that even critics rely on when they invoke settled doctrine. Reading it as pure rope erases the asymmetric extraction (supermajority-ratcheted costs borne by the excluded) that the victim declarations make visible. The theater_ratio trajectory is the early-warning instrument: if the method's constraint function fully atrophies into post-hoc justification while the fidelity performance continues, the arrangement drifts toward piton — administered by justices who could change it, borne diffusely by litigants, maintained because the cost of replacement exceeds what any single administrator bears.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This constraint is one reading (originalist_reading) of the kernel constitutional_text_authority; what would a sibling reading change structurally, and where exactly is the disagreement located?',
    'Comparative classification of the sibling files (living_constitutionalist_reading, positivist_reading): the disagreement is located in the SOURCE of semantic authority — historical public understanding (this reading) versus contemporary moral principles versus formal enactment validity. Sibling adoption reassigns the victim set and re-bases epsilon.',
    'Adopting the living reading dissolves the historical-evidence gate and converts current victims (change-seekers) into beneficiaries while exposing settlement holders; adopting the positivist reading removes the moral-content question entirely and re-keys validity to enactment procedure, shrinking the victim set to procedural outsiders.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: this file instantiates one reading of a three-reading kernel; sibling readings are separate constraints.').

omega_variable(
    historical_record_indeterminacy,
    'How determinate is the ratification-era public understanding that functions as the gate — does historical evidence constrain outcomes, or does it delegate discretion to whichever historians and corpus-linguistic selectors the opinion cites?',
    'Inter-rater reliability studies of originalist evidence use: independent teams applying stated historical methods to the same questions, measuring convergence on permissible outcomes; audit of cherry-picking rates in law-office history.',
    'High indeterminacy means the gate hides discretion rather than removing it — effective extraction rises (discretion migrates to evidence-selectors aligned with preferred outcomes) and the constraint slides toward the snare boundary; low indeterminacy supports the coordination framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_record_indeterminacy, empirical, 'Whether the historical-evidence gate is determinate or discretionary in operation.').

omega_variable(
    method_constraint_vs_outcome_divergence,
    'Does the originalist method actually change which outcomes courts reach, or does it relabel outcomes reached on other grounds?',
    'Outcome-prediction studies modeling votes under originalist-gated versus method-neutral features; natural experiments where the same legal question is decided under differing methodological regimes.',
    'If outcomes are predicted equally well without the historical variables, the coordination function is substantially theatrical, theater_ratio is understated, and the constraint degrades toward piton (performance of fidelity) or snare (cover for results-driven adjudication).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(method_constraint_vs_outcome_divergence, empirical, 'Whether the gate constrains outcomes or merely furnishes post-hoc justification.').

omega_variable(
    amendment_threshold_cost_classification,
    'Is the cost imposed on change-seekers by routing all semantic change through Article V a legitimate price of popular sovereignty, or an extraction device that freezes incumbent settlements behind a supermajority wall?',
    'Normative analysis combined with comparative data: amendment success rates for rights-expanding versus rights-consolidating proposals, and cross-national comparison of constitutional change costs.',
    'If the threshold is legitimate democratic control, victim costs are coordination overhead and the rope side of the classification strengthens; if it systematically prices out the excluded, the extraction side dominates and the classification trends toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(amendment_threshold_cost_classification, preference, 'Whether the Article V routing cost is coordination overhead or asymmetric extraction.').

omega_variable(
    methodological_identity_lock_durability,
    'Are committed originalist justices and scholars identity-locked such that disconfirming evidence (indeterminacy findings, outcome-divergence results) cannot move their methodological allegiance?',
    'Longitudinal tracking of methodological switching following major indeterminacy revelations or reputational shocks; career-path analysis of scholars who publicly abandon the method.',
    'If locked, enforcement persists independently of evidence quality, raising durable suppression and hardening the tangled_rope classification; if revisable, the constraint retains a self-correction channel that lowers long-run extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(methodological_identity_lock_durability, empirical, 'Durability of identity fusion among the method''s professional carriers.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_text_authority__originalist_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t0, constitutional_text_authority__originalist_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(cons_tr_t10, constitutional_text_authority__originalist_reading, theater_ratio, 10, 0.22).
narrative_ontology:measurement(cons_tr_t20, constitutional_text_authority__originalist_reading, theater_ratio, 20, 0.28).
narrative_ontology:measurement(cons_tr_t30, constitutional_text_authority__originalist_reading, theater_ratio, 30, 0.33).
narrative_ontology:measurement(cons_tr_t40, constitutional_text_authority__originalist_reading, theater_ratio, 40, 0.38).
narrative_ontology:measurement(cons_tr_t50, constitutional_text_authority__originalist_reading, theater_ratio, 50, 0.42).

% Extraction over time
narrative_ontology:measurement(cons_be_t0, constitutional_text_authority__originalist_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(cons_be_t10, constitutional_text_authority__originalist_reading, base_extractiveness, 10, 0.42).
narrative_ontology:measurement(cons_be_t20, constitutional_text_authority__originalist_reading, base_extractiveness, 20, 0.48).
narrative_ontology:measurement(cons_be_t30, constitutional_text_authority__originalist_reading, base_extractiveness, 30, 0.53).
narrative_ontology:measurement(cons_be_t40, constitutional_text_authority__originalist_reading, base_extractiveness, 40, 0.57).
narrative_ontology:measurement(cons_be_t50, constitutional_text_authority__originalist_reading, base_extractiveness, 50, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t0, constitutional_text_authority__originalist_reading, suppression_requirement, 0, 0.44).
narrative_ontology:measurement(cons_su_t10, constitutional_text_authority__originalist_reading, suppression_requirement, 10, 0.49).
narrative_ontology:measurement(cons_su_t20, constitutional_text_authority__originalist_reading, suppression_requirement, 20, 0.53).
narrative_ontology:measurement(cons_su_t30, constitutional_text_authority__originalist_reading, suppression_requirement, 30, 0.57).
narrative_ontology:measurement(cons_su_t40, constitutional_text_authority__originalist_reading, suppression_requirement, 40, 0.6).
narrative_ontology:measurement(cons_su_t50, constitutional_text_authority__originalist_reading, suppression_requirement, 50, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_text_authority__originalist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(constitutional_text_authority__originalist_reading, living_constitutionalist_reading).
narrative_ontology:affects_constraint(constitutional_text_authority__originalist_reading, positivist_reading).
narrative_ontology:affects_constraint(constitutional_text_authority__originalist_reading, article_v_amendment_supermajority).

% DUAL FORMULATION NOTE:
% Constraint family decomposition per the epsilon-invariance principle: the colloquial label 'constitutional interpretation' covers three structurally distinct authority regimes that cannot share one constraint story, because each assigns semantic authority to a different source and therefore generates a different victim set and a different epsilon. This file instantiates the originalist reading; the living-constitutionalist and positivist readings are sibling files. The upstream/downstream structure runs through article_v_amendment_supermajority: the originalist gate's extraction mechanism depends on the amendment threshold's cost, and the sibling readings stand in typed relations declared in cs_structure.reading_relations. Linkage is bidirectional by family rule — each sibling links back to this constraint_id.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
