% ============================================================================
% CONSTRAINT STORY: us_constitution_interpretive__originalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_constitution_interpretive__originalist_reading, []).

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
 *   constraint_id: us_constitution_interpretive__originalist_reading
 *   human_readable: Originalist Reading of the U.S. Constitution: Meaning Fixed at Ratification
 *   domain: legal/political
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested kernel 'how the U.S.
 *   Constitution acquires operative meaning': the originalist reading, under
 *   which constitutional meaning was fixed at ratification and interpretive
 *   authority flows from fidelity to the framers' intent or the founding
 *   era's original public meaning. The living-constitution and
 *   popular-constitutionalism readings are separate constraints in separate
 *   files; nothing about them belongs inside this story's classification. The
 *   epsilon referent is the standing arrangement under contest —
 *   constitutional adjudication conducted under the fixed-meaning discipline,
 *   as it has actually operated during the method's four-decade consolidation
 *   — assessed honestly, including its declared victims; the reading's
 *   adherents would count some of the same costs as the legitimate price of
 *   intergenerational commitment rather than as extraction, and that value
 *   disagreement is carried in the dead_hand_legitimacy omega rather than
 *   averaged away here. Structurally the reading presents itself as a
 *   restored natural baseline ('texts mean what they said when adopted'), yet
 *   identifiable beneficiaries and victims ride the same discipline, which is
 *   why this file claims tangled_rope with independently authored metrics.
 *   KEY AGENTS (by structural relationship): - scotus_originalist_majority:
 *   primary administrator (institutional/identity_locked) — sets and applies
 *   the method - conservative_legal_movement: concentrated collector of gains
 *   plus enforcement infrastructure (institutional/identity_locked) -
 *   federalism_advocates, property_rights_defenders,
 *   religious_liberty_claimants_original_understanding: beneficiary coalition
 *   (organized; mobile/arbitrage/constrained exits) -
 *   unenumerated_rights_claimants: primary target (powerless/trapped) -
 *   federal_regulatory_expansion_advocates: secondary target
 *   (organized/constrained) - present_day_citizens: diffuse dead-hand target
 *   (powerless/trapped) - dynamic_interpretation_practitioners: suppressed
 *   rival readers (institutional/excluded) -
 *   comparative_public_law_observers: analytical observer
 *   (analytical/analytical). Family links run to both sibling readings' files
 *   via network.affects_constraints.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_interpretive__originalist_reading, 0.6).
domain_priors:suppression_score(us_constitution_interpretive__originalist_reading, 0.65).
domain_priors:theater_ratio(us_constitution_interpretive__originalist_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_interpretive__originalist_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(us_constitution_interpretive__originalist_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(us_constitution_interpretive__originalist_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_interpretive__originalist_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(us_constitution_interpretive__originalist_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_interpretive__originalist_reading, tangled_rope).
narrative_ontology:human_readable(us_constitution_interpretive__originalist_reading, "Originalist Reading of the U.S. Constitution: Meaning Fixed at Ratification").
narrative_ontology:topic_domain(us_constitution_interpretive__originalist_reading, "legal/political").

domain_priors:requires_active_enforcement(us_constitution_interpretive__originalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_interpretive__originalist_reading, '718eb688-222e-43ff-b708-732a0f6ef19f').
narrative_ontology:cs_kernel_codification('718eb688-222e-43ff-b708-732a0f6ef19f', fixed_text).
narrative_ontology:cs_authority_grounding('718eb688-222e-43ff-b708-732a0f6ef19f', lineage).
narrative_ontology:cs_interpretation_layer_present('718eb688-222e-43ff-b708-732a0f6ef19f').
narrative_ontology:cs_reading_relation('718eb688-222e-43ff-b708-732a0f6ef19f', us_constitution_interpretive__living_constitution_reading, forecloses).
narrative_ontology:cs_reading_relation('718eb688-222e-43ff-b708-732a0f6ef19f', us_constitution_interpretive__popular_constitutionalism_reading, influences).
narrative_ontology:cs_axiom('718eb688-222e-43ff-b708-732a0f6ef19f', foundational, constitutional_meaning_fixed_at_ratification).
narrative_ontology:cs_axiom_status(constitutional_meaning_fixed_at_ratification, holdable).
narrative_ontology:cs_axiom_grounding('718eb688-222e-43ff-b708-732a0f6ef19f', constitutional_meaning_fixed_at_ratification, conventional).
narrative_ontology:cs_axiom('718eb688-222e-43ff-b708-732a0f6ef19f', foundational, interpretive_authority_derives_from_fidelity_to_original_meaning).
narrative_ontology:cs_axiom_status(interpretive_authority_derives_from_fidelity_to_original_meaning, holdable).
narrative_ontology:cs_axiom_grounding('718eb688-222e-43ff-b708-732a0f6ef19f', interpretive_authority_derives_from_fidelity_to_original_meaning, conventional).
narrative_ontology:cs_axiom('718eb688-222e-43ff-b708-732a0f6ef19f', secondary, article_v_exclusive_revision_channel).
narrative_ontology:cs_axiom_status(article_v_exclusive_revision_channel, holdable).
narrative_ontology:cs_axiom_grounding('718eb688-222e-43ff-b708-732a0f6ef19f', article_v_exclusive_revision_channel, conventional).
narrative_ontology:cs_reference_frame('718eb688-222e-43ff-b708-732a0f6ef19f', ratification_era_public_meaning_baseline).
narrative_ontology:cs_drift_state('718eb688-222e-43ff-b708-732a0f6ef19f', contemporary_consolidated_bench_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('718eb688-222e-43ff-b708-732a0f6ef19f', '').
narrative_ontology:cs_kernel_id(us_constitution_interpretive__originalist_reading, us_constitution_interpretive).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__originalist_reading, scotus_originalist_majority).
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__originalist_reading, conservative_legal_movement).
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__originalist_reading, federalism_advocates).
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__originalist_reading, property_rights_defenders).
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__originalist_reading, religious_liberty_claimants_original_understanding).
narrative_ontology:constraint_victim(us_constitution_interpretive__originalist_reading, unenumerated_rights_claimants).
narrative_ontology:constraint_victim(us_constitution_interpretive__originalist_reading, federal_regulatory_expansion_advocates).
narrative_ontology:constraint_victim(us_constitution_interpretive__originalist_reading, present_day_citizens).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Controls the Supreme Court's docket and writes the opinions that apply fixed-meaning method to live disputes. Its members reached their seats through a selection process explicitly filtered for commitment to the method; departing from it publicly would cost them standing with the coalition that elevated them. The method concentrates interpretive authority in courts that share their commitments.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__originalist_reading, scotus_originalist_majority, agenda_setter,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(us_constitution_interpretive__originalist_reading, scotus_originalist_majority, beneficiary).

% A network of scholars, judges, litigators, and training institutions that produces the founding-era historical canon, screens candidates for judicial appointment, and places allied clerks throughout the judiciary. Careers, appointments, publishing platforms, and donor funding scale with the method's dominance; members' professional identities are constituted through the project itself. The gains flowing from the arrangement's operation accrue visibly to this seat: staffing pipelines, institutional influence, and control over which historical claims become operative.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__originalist_reading, conservative_legal_movement, beneficiary,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(us_constitution_interpretive__originalist_reading, conservative_legal_movement, agenda_setter).

% State attorneys general, state-sovereignty litigants, and allied think tanks who collect doctrinal wins whenever federal power is read down to the 1788 settlement. If returns from this interpretive approach fell, they could redirect effort toward state-law and interstate-coordination strategies without losing their standing elsewhere.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__originalist_reading, federalism_advocates, beneficiary,
    organized, biographical, mobile, national).

% Landowners, developers, and business coalitions that fund takings, contract-clause, and regulatory-limitation litigation keyed to founding-era protections. They collect enforcement of eighteenth-century property settlements against modern regulation, and their capital mobility gives them fallback venues if any particular front closes.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__originalist_reading, property_rights_defenders, beneficiary,
    organized, biographical, arbitrage, national).

% Religious institutions and communities whose claims are framed to the founding-era understanding of free exercise and establishment. They collect protections calibrated to 1791 practice, which serves some contemporary claims well and others poorly. Their alternatives run mainly through statute and state law, at higher cost and lower durability.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__originalist_reading, religious_liberty_claimants_original_understanding, beneficiary,
    organized, biographical, constrained, national).

% Persons asserting dignitary, bodily-autonomy, privacy, and equal-citizenship interests that have no anchor in the ratified catalogue of rights. To advance a claim they must plead a founding-era history in which people like them were excluded from the franchise and the record. When the method retires precedent built on other approaches, they bear the lost protection directly, and there is no exit from the jurisdiction of the document that governs them.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__originalist_reading, unenumerated_rights_claimants, payer,
    powerless, biographical, trapped, national).

% Federal agencies, agency-aligned litigators, and public-interest regulatory coalitions that confront limits drawn from the founding settlement — narrow constructions of executive and congressional authority, major-questions reasoning, and reserved-state spheres. They retain partial alternatives in tighter statutory drafting and state-level regulation, but each carries materially higher cost and slower deployment.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__originalist_reading, federal_regulatory_expansion_advocates, payer,
    organized, biographical, constrained, national).

% The living population, governed by a settlement ratified by a franchise that included almost none of them. Revision runs exclusively through an amendment procedure requiring supermajorities that in practice almost never assemble. They bear the costs of decisions locked to a different century's compromises diffusely and without any individual lever, short of emigration, to escape the governing text.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__originalist_reading, present_day_citizens, payer,
    powerless, biographical, trapped, national).

% Judges, academics, and lawyers committed to reading the text in light of changing conditions. Under consolidated appointment filtering they are progressively shut out of the benches where interpretation happens; their scholarship circulates outside the operative canon; and their objections register in the system chiefly as losing votes and dissents rather than as seated participation in deciding what the text requires.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__originalist_reading, dynamic_interpretation_practitioners, excluded,
    institutional, biographical, constrained, national).

% Scholars tracking how the United States' fixed-meaning adjudication compares with other written-constitution systems that permit formal revision or pluralist reading. They maintain the external record of how the arrangement performs over time and across jurisdictions, and hold no stake in any domestic coalition's outcome.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__originalist_reading, comparative_public_law_observers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(us_constitution_interpretive__originalist_reading, conservative_legal_movement).
narrative_ontology:fixing_cost_class(us_constitution_interpretive__originalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared, durable rule for settling what the Constitution requires: instead of each generation and each institution renegotiating the fundamentals, everyone adjudicates against a fixed published baseline. This stabilizes legal expectations, lets long-horizon planning and reliance form around known rules, and gives competing institutions a common arbiter for disputes they cannot otherwise resolve.
% TRANSFER_FUNCTION: Moves interpretive authority and doctrinal outcomes from present majorities and from claimants whose interests lack textual anchors, toward the settlement ratified in 1787-1791 and the constituencies its terms favored — propertied interests, state prerogatives, and the religious-liberties shape of the founding era. It also moves discretion out of judges' hands and into the historical record, concentrating the remaining discretion in whoever commands that record.
% ABSENT_VOICES: Those with no seat at ratification and their successors — enslaved persons, women, the non-propertied — together with today's unenumerated-rights claimants, would contest the authority of a settlement drafted and adopted without them. They stand outside the courtroom record the method consults: their objection enters the system only as a litigant pleading a history that excludes her, or through political channels the arrangement is built to bypass. Practitioners of rival reading methods are likewise present only as dissents, having been filtered out of the appointing stream.
% DISAPPEARANCE_RATIONALE: If the fixed-meaning discipline vanished overnight, courts would revert to pluralist method, the justification structure behind the recent run of history-grounded rulings would dissolve, appointment politics would reorganize around a different selection criterion, and reliance interests formed on the assumption that old meanings stay put would unwind as doctrine resumed moving with conditions. The beneficiary coalition's staffing, canon, and litigation strategy would all be rebuilt from scratch.
% FOUNDING_PROBLEM: Secure government under a written supreme law against judges substituting their own preferences for the law's commands — and hold the polity to the bargain its ratifiers actually struck, so that legitimacy rests on consent given at adoption rather than on whatever any sitting interpreter prefers.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting coalition: legal-historical scholarship spanning methodological camps — including scholars hostile to the method — documents the founding generation's commitment to published, fixed meaning and the enduring problem of judicial discretion it was meant to answer. Living-constitution and popular-constitutional theorists independently attest that the problem of unelected judges overriding democratic preference is real; they dispute only whether freezing meaning is the right cure. No attestation of the founding problem relies solely on the beneficiary coalition's own account.
narrative_ontology:disappearance_verdict(us_constitution_interpretive__originalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(us_constitution_interpretive__originalist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_interpretive__originalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(us_constitution_interpretive__originalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(us_constitution_interpretive__originalist_reading, 0.6, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_interpretive__originalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_constitution_interpretive__originalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(us_constitution_interpretive__originalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.60: the discipline transfers real, bounded value — enumerated-catalogue limitation strips protections previously extended by other methods, founding-settlement constructions curtail federal regulatory capacity, and the whole living population is governed by a franchise that excluded nearly all of them — but the transfer is capped by what the ratified text actually contains; this is not open-ended predation, which keeps epsilon below snare territory. Suppression is 0.65 and is deliberately authored as a RAW structural quantity: it reflects the enforcement machinery the method requires (appointment filtering, canon control, retirement of rival-method precedent, professional sanction of non-conforming scholarship), not any scaled effective figure — the engine alone scales extractiveness by directionality and scope. Theater_ratio is 0.48: the core function (adjudicating against a fixed baseline) is real, but an industrialized volume of founding-era argument — dictionary citations, selective source compilation, result-shaped histories — now accompanies it, and a growing share of that activity is performance serving predetermined conclusions. Accessibility_collapse is 0.60: once the method consolidates, litigants must frame claims in founding-era terms to win, and rival-method precedent falls, yet genuine alternatives persist (state constitutions, statute, Article V amendment, political mobilization), so alternatives are heavily narrowed but not eliminated. Resistance is 0.60: the academic majority, a persistent bench minority, and recurring popular challenges (court-expansion proposals, reform movements) contest the method continuously. The three temporal series share one time grid (points 0,9,18,27,36,45 mapping roughly 1980-2025) so no series borrows another's endpoint. Trajectories are monotonic consolidation rather than cyclical: extraction accumulation tracks the compounding distributional wins of a maturing governing method, suppression_requirement tracks the enforcement ratchet (escalating confirmation wars, a matured selection pipeline, systematic precedent retirement), and theater tracks the industrialization of historical argument. Coalition potential among the payers is real but presently unrealized: unenumerated-rights claimants and regulatory-capacity advocates lose through different doctrinal doors and have not combined, which is precisely what keeps each seat's computed burden high.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute sharply divergent types from identical structural data. From the trapped payer seats — unenumerated_rights_claimants and present_day_citizens — the arrangement computes as enforced loss with no exit: whatever coordination value exists, they experience only the stripped protections and the dead hand. From the arbitrage-grade beneficiary seat (property_rights_defenders) the same structure computes benignly, close to ordinary reliance protection, because exit dampens experienced extraction toward subsidy. From the identity_locked administrator and movement seats the structure computes as duty and restoration — they are not extracting in their own account; they are enforcing a promise. Same-power divergence is visible inside the organized tier: federal_regulatory_expansion_advocates (payer) and federalism_advocates (beneficiary) hold comparable resources and standing, but their exits differ — constrained versus mobile — and their declared positions differ, so the engine should place them at opposite ends of the directionality axis despite equal nominal power. Inter-institutionally, courts administering the method, federal agencies constrained by it, and states sheltered by it occupy the same legal order with opposite directionalities. The engine computes all of this per seat; the authored claim adjudicates none of it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations drive the derivation. The beneficiary coalition (movement, federalism advocates, property defenders, original-understanding religious claimants, and the bench that administers the method while collecting its concentrated authority) sits near the beneficiary end; property defenders' arbitrage exit pushes them furthest toward d≈0, while the bench's agenda-setting role is captured by its secondary beneficiary declaration plus its identity-locked exit rather than by an override — the derivation chain already separates administration from collection here. Trapped payers sit near the full-target end: unenumerated_rights_claimants and present_day_citizens carry high d amplified by their inability to leave the governing jurisdiction, and national scope modestly amplifies effective extraction for all seats by hardening verification. Dynamic_interpretation_practitioners are excluded rather than coordinated — their marginalization is part of the enforcement surface, not a cost they voluntarily bear. No directionality_overrides were needed: every seat's true relationship to the discipline is already expressed by its role declaration plus exit options.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope claim is what keeps both mislabelings visible at once. Reading the arrangement as pure extraction (snare) would erase the genuine coordination function — a stable rule of recognition that solves the collective-action problem of perpetual renegotiation of fundamentals and underwrites reliance — which is real and which even many payers implicitly rely on. Reading it as pure coordination (rope) would erase the asymmetric allocation running through the same channel: the identical discipline that stabilizes also routes doctrinal outcomes to the founding settlement's favored constituencies and strips claimants the settlement never counted, sustained by active enforcement rather than by participant preference. On genealogy: the founding problem (judicial discretion versus democratic legitimacy under written law) is corroborated live from outside the beneficiary coalition, so mandatrophy_resolved stays false and no zombie flag is warranted on the mismatch path. The forward risk vector is piton drift rather than obsolescence: if the enforcement apparatus ever became pure canon maintenance — historical performance defending a discipline nobody's outcomes depend on — theater_ratio would climb past functional activity while the cost-to-fix stayed prohibitive; the current 0.48 with rising trend is the early signature worth watching.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_structure,
    'This constraint is one reading (originalist_reading) of the kernel us_constitution_interpretive. What structurally changes if a sibling reading (living_constitution_reading, popular_constitutionalism_reading) governed instead?',
    'Read the sibling constraint stories directly: each sibling file carries its own epsilon, beneficiary/victim structure, and claimed type for the arrangement its reading instantiates; comparison across the family is cross-file, never intra-file.',
    'Under the living reading the victim set shifts (historical-fidelity losers gain; adaptability-dependent claimants lose), judicial power widens rather than narrows, and enforcement targets different dissent. Under the popular reading the agenda-setter seat moves from benches to movements entirely. None of these variants belong inside this file''s classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Committer-frame routing: this story is one sibling of a three-reading kernel; sibling deltas live in their own files.').

omega_variable(
    cs_framing_text_vs_legitimacy_claim,
    'Is the kernel of this commitment system the fixed 1787 text itself, or the legitimacy claim layered above it — the promise that popular sovereignty froze a higher law that binds later majorities?',
    'Test both framings against the enforcement record: if discipline survives where textual fidelity is unverifiable (vague clauses, thin historical records) but never survives where the frozen-bargain story loses political salience, the operative kernel is the legitimacy claim, not the text.',
    'Framing the kernel as the text supports a formalized/fixed-text commitment structure; framing it as the frozen-bargain legitimacy claim makes the constraint more constructed and more extractive-prone, since the claim can be maintained theatrically after textual fidelity stops mattering. Classification of the enforcement surface shifts accordingly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cs_framing_text_vs_legitimacy_claim, conceptual, 'CS-framing under-determination: obvious framing (fixed text) versus layered framing (ratified-bargain legitimacy claim).').

omega_variable(
    semantic_fixation_of_indeterminate_clauses,
    'Can ''meaning fixed at ratification'' hold coherently for the Constitution''s indeterminate clauses, or does fixation silently degrade into application-fixation — freezing not what the words conveyed but whichever operationalization first attached?',
    'Philosophical-linguistic analysis distinguishing semantic content from doctrinal application clause by clause, checked against whether founding-era usage actually determined the disputed provisions'' extension.',
    'If fixation fails for indeterminate clauses, a large share of the method''s output rests on arbitrary anchors dressed as history: measured theater rises, the coordination function weakens, and effective extraction climbs because payers receive rigidity without determinacy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(semantic_fixation_of_indeterminate_clauses, conceptual, 'Whether the reading''s core semantic premise survives contact with constitutional vagueness.').

omega_variable(
    dead_hand_legitimacy,
    'Is governance by a ratified settlement — adopted by a franchise excluding nearly everyone then living and everyone since — legitimate intergenerational commitment binding the present, or extraction from the unborn and the unenfranchised?',
    'Not resolvable by data alone: it turns on the weight a polity owes its founding compact versus present-majority consent. Comparative evidence (amendment frequency, successor-regime treatment of inherited constitutions) bounds the argument; the verdict is a values judgment.',
    'If the dead-hand component is legitimate commitment, part of the measured extractiveness is misattributed and true epsilon drops; if it is extraction, the present_day_citizens victim declaration understates the burden and the reading''s own accounting of its costs is systematically flattering.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(dead_hand_legitimacy, preference, 'The dead-hand dispute: commitment versus extraction across generations.').

omega_variable(
    history_argument_good_faith_share,
    'What fraction of the founding-era historical argument deployed in litigation is genuine method application versus result-driven source selection assembled to justify predetermined conclusions?',
    'Blind expert review: historians unbriefed on the desired outcome evaluate the provenance and representativeness of the historical corpora used in landmark filings, benchmarked against neutral scholarly syntheses of the same questions.',
    'Recalibrates theater_ratio directly: a high result-selection share confirms the rising-theater trajectory as proxy displacement (Goodhart drift on ''fidelity''), while a low share supports treating the historical apparatus as functional and lowers the piton-drift estimate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(history_argument_good_faith_share, empirical, 'Good-faith versus result-shaped historical method in operative filings.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the measured suppression of rival interpretive practice predominantly structural (appointment filtering, bench composition, canon control) or partly internalized (professionals'' self-censorship and judges'' fused self-conception as faithful executors of the founding bargain)?',
    'Post-turnover trajectory test: track whether conformist pressure persists in institutions where the appointment filter has been removed or reversed — persistent conformity after the structural filter lifts indicates internalized suppression; immediate reversion indicates structural.',
    'If internalized components are large, effective suppression outlasts the machinery that produced it and the arrangement resists reform beyond what bench composition alone predicts; if small, restoring pluralist selection would rapidly reopen the interpretive field.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Structural versus internalized share of the enforcement burden on rival readers.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_interpretive__originalist_reading, 0, 45).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_const_orig_tr_t0, us_constitution_interpretive__originalist_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(us_const_orig_tr_t9, us_constitution_interpretive__originalist_reading, theater_ratio, 9, 0.3).
narrative_ontology:measurement(us_const_orig_tr_t18, us_constitution_interpretive__originalist_reading, theater_ratio, 18, 0.35).
narrative_ontology:measurement(us_const_orig_tr_t27, us_constitution_interpretive__originalist_reading, theater_ratio, 27, 0.4).
narrative_ontology:measurement(us_const_orig_tr_t36, us_constitution_interpretive__originalist_reading, theater_ratio, 36, 0.44).
narrative_ontology:measurement(us_const_orig_tr_t45, us_constitution_interpretive__originalist_reading, theater_ratio, 45, 0.48).

% Extraction over time
narrative_ontology:measurement(us_const_orig_be_t0, us_constitution_interpretive__originalist_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(us_const_orig_be_t9, us_constitution_interpretive__originalist_reading, base_extractiveness, 9, 0.42).
narrative_ontology:measurement(us_const_orig_be_t18, us_constitution_interpretive__originalist_reading, base_extractiveness, 18, 0.47).
narrative_ontology:measurement(us_const_orig_be_t27, us_constitution_interpretive__originalist_reading, base_extractiveness, 27, 0.52).
narrative_ontology:measurement(us_const_orig_be_t36, us_constitution_interpretive__originalist_reading, base_extractiveness, 36, 0.56).
narrative_ontology:measurement(us_const_orig_be_t45, us_constitution_interpretive__originalist_reading, base_extractiveness, 45, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(us_const_orig_su_t0, us_constitution_interpretive__originalist_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(us_const_orig_su_t9, us_constitution_interpretive__originalist_reading, suppression_requirement, 9, 0.47).
narrative_ontology:measurement(us_const_orig_su_t18, us_constitution_interpretive__originalist_reading, suppression_requirement, 18, 0.53).
narrative_ontology:measurement(us_const_orig_su_t27, us_constitution_interpretive__originalist_reading, suppression_requirement, 27, 0.58).
narrative_ontology:measurement(us_const_orig_su_t36, us_constitution_interpretive__originalist_reading, suppression_requirement, 36, 0.62).
narrative_ontology:measurement(us_const_orig_su_t45, us_constitution_interpretive__originalist_reading, suppression_requirement, 45, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_interpretive__originalist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(us_constitution_interpretive__originalist_reading, us_constitution_interpretive__living_constitution_reading).
narrative_ontology:affects_constraint(us_constitution_interpretive__originalist_reading, us_constitution_interpretive__popular_constitutionalism_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'how the Constitution is interpreted' decomposes into three structurally distinct arrangements per the epsilon-invariance principle. The originalist reading (this file) instantiates fixed-meaning adjudication with a beneficiary coalition of founding-settlement constituencies and trapped payers lacking textual anchors; the living_constitution_reading instantiates adaptive adjudication with a different victim set (historical-fidelity constituencies) and wider judicial power; popular_constitutionalism_reading relocates the agenda-setter seat from benches to popular movements altogether. Their epsilons differ because their beneficiary/victim structures differ — they are not one constraint viewed from angles. Edges run from this file to both siblings because the originalist consolidation changes the operating environment of each: it raises the legitimacy cost of adaptive reading and shrinks the institutional channel through which popular contestation reaches constitutional meaning, without logically eliminating either sibling's position.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
