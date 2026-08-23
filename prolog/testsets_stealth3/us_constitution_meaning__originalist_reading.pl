% ============================================================================
% CONSTRAINT STORY: us_constitution_meaning__originalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_constitution_meaning__originalist_reading, []).

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
 *   constraint_id: us_constitution_meaning__originalist_reading
 *   human_readable: Originalist Fixation: Constitutional Meaning Fixed at Ratification-Era Public Meaning
 *   domain: constitutional/legal/political_philosophy
 *
 * SUMMARY:
 *   A dominant interpretive regime instructs federal judges that each
 *   provision of the United States Constitution means what its words publicly
 *   meant to the ratifying public at adoption (1788 for the original text,
 *   each amendment's own adoption date thereafter), and that contemporary
 *   moral and social understandings carry no authority over that meaning —
 *   present circumstances matter only at the margin of applying fixed
 *   concepts to new facts. The regime is maintained by no single statute but
 *   by an enforcement ecology: nomination and confirmation screening for
 *   demonstrated fidelity, a scholarly apparatus producing ratification-era
 *   evidence, professional incentives rewarding adherence, and the structural
 *   fact that adherents occupy life-tenured seats. Its coordination promise
 *   is a constitution that constrains rather than mirrors whoever sits on the
 *   bench; its costs concentrate on claimants whose sought-after protections
 *   lack historical warrants and on communities whose constitutional
 *   understandings never entered the record that serves as the sole
 *   evidentiary authority. KEY AGENTS (by structural relationship): -
 *   originalist_aligned_justices: Agenda-setting enforcers
 *   (institutional/identity_locked) — administer the fixed-meaning test from
 *   life-tenured seats - confirmation_gatekeepers: Secondary agenda-setters
 *   (powerful/arbitrage) — screen nominees for fidelity and deploy the
 *   doctrine instrumentally - counter_majoritarian_constraint_advocates:
 *   Primary beneficiaries (organized/mobile) — receive placements,
 *   institutional growth, and doctrinal authority -
 *   originalist_scholarship_community: Beneficiaries with maintenance duties
 *   (organized/identity_locked) — supply the evidentiary base; careers ride
 *   on the doctrine - rights_claimants_without_historical_support: Primary
 *   targets (powerless/trapped) — lose adjudicative recognition for
 *   unhistorically warranted claims -
 *   communities_unrepresented_at_ratification: Targets excluded at the source
 *   (powerless/trapped) — their understandings never entered the
 *   authoritative record - nonoriginalist_judicial_dissenters: Resisting
 *   payers (moderate/identity_locked) — their outputs are screened out and
 *   devalued - present_day_electorate: Near-symmetric seat (organized/mobile)
 *   — gains agenda control over change, pays when popular protections find no
 *   historical warrant - constitutional_litigators: Dual-position
 *   professionals (moderate/constrained) — sell compliance with the test and
 *   absorb its losses
 *
 * KEY AGENTS:
 *   - - originalist_aligned_justices: Agenda-setting enforcer (institutional/identity_locked) — administers the fixed-meaning test from life-tenured seats
 *   - - confirmation_gatekeepers: Secondary agenda-setter (powerful/arbitrage) — screens nominees for demonstrated fidelity; deploys the doctrine instrumentally
 *   - - counter_majoritarian_constraint_advocates: Primary beneficiary (organized/mobile) — receives placements, institutional growth, and doctrinal authority
 *   - - originalist_scholarship_community: Beneficiary with maintenance duties (organized/identity_locked) — supplies the ratification-era evidentiary base; careers ride on continued operation
 *   - - rights_claimants_without_historical_support: Primary target (powerless/trapped) — bears denial of adjudicative recognition for claims lacking historical warrants
 *   - - communities_unrepresented_at_ratification: Target excluded at the source (powerless/trapped) — its members' constitutional understandings were never recorded in the authoritative evidence
 *   - - nonoriginalist_judicial_dissenters: Resisting payer (moderate/identity_locked) — bears devaluation of its jurisprudence and exclusion from nomination pipelines
 *   - - present_day_electorate: Near-symmetric seat (organized/mobile) — holds the amendment-and-politics channel the constraint directs change through, and pays when popular protections find no historical warrant
 *   - - constitutional_litigators: Dual-position professional seat (moderate/constrained) — profits from compliance demand and absorbs the test's losses for clients
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_meaning__originalist_reading, 0.62).
domain_priors:suppression_score(us_constitution_meaning__originalist_reading, 0.75).
domain_priors:theater_ratio(us_constitution_meaning__originalist_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_meaning__originalist_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(us_constitution_meaning__originalist_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(us_constitution_meaning__originalist_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_meaning__originalist_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(us_constitution_meaning__originalist_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_meaning__originalist_reading, tangled_rope).
narrative_ontology:human_readable(us_constitution_meaning__originalist_reading, "Originalist Fixation: Constitutional Meaning Fixed at Ratification-Era Public Meaning").
narrative_ontology:topic_domain(us_constitution_meaning__originalist_reading, "constitutional/legal/political_philosophy").

domain_priors:requires_active_enforcement(us_constitution_meaning__originalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_meaning__originalist_reading, 'a6ea163a-896d-43c4-b33e-9b33db342cee').
narrative_ontology:cs_kernel_codification('a6ea163a-896d-43c4-b33e-9b33db342cee', fixed_text).
narrative_ontology:cs_authority_grounding('a6ea163a-896d-43c4-b33e-9b33db342cee', lineage).
narrative_ontology:cs_interpretation_layer_present('a6ea163a-896d-43c4-b33e-9b33db342cee').
narrative_ontology:cs_reading_relation('a6ea163a-896d-43c4-b33e-9b33db342cee', us_constitution_meaning__living_constitutionalist_reading, forecloses).
narrative_ontology:cs_reading_relation('a6ea163a-896d-43c4-b33e-9b33db342cee', us_constitution_meaning__positivist_reading, coexists_with).
narrative_ontology:cs_axiom('a6ea163a-896d-43c4-b33e-9b33db342cee', foundational, fixed_ratification_meaning_is_judicially_binding).
narrative_ontology:cs_axiom_status(fixed_ratification_meaning_is_judicially_binding, holdable).
narrative_ontology:cs_axiom_grounding('a6ea163a-896d-43c4-b33e-9b33db342cee', fixed_ratification_meaning_is_judicially_binding, conventional).
narrative_ontology:cs_axiom('a6ea163a-896d-43c4-b33e-9b33db342cee', foundational, judicial_meaning_invention_is_popular_sovereignty_usurpation).
narrative_ontology:cs_axiom_status(judicial_meaning_invention_is_popular_sovereignty_usurpation, holdable).
narrative_ontology:cs_axiom_grounding('a6ea163a-896d-43c4-b33e-9b33db342cee', judicial_meaning_invention_is_popular_sovereignty_usurpation, deontological).
narrative_ontology:cs_reference_frame('a6ea163a-896d-43c4-b33e-9b33db342cee', fixed_ratification_era_public_meaning).
narrative_ontology:cs_drift_state('a6ea163a-896d-43c4-b33e-9b33db342cee', contemporary_doctrine_2020s, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a6ea163a-896d-43c4-b33e-9b33db342cee', '').
narrative_ontology:cs_kernel_id(us_constitution_meaning__originalist_reading, us_constitution_meaning).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_meaning__originalist_reading, counter_majoritarian_constraint_advocates).
narrative_ontology:constraint_beneficiary(us_constitution_meaning__originalist_reading, originalist_scholarship_community).
narrative_ontology:constraint_victim(us_constitution_meaning__originalist_reading, rights_claimants_without_historical_support).
narrative_ontology:constraint_victim(us_constitution_meaning__originalist_reading, communities_unrepresented_at_ratification).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(us_constitution_meaning__originalist_reading, present_day_electorate).
narrative_ontology:constraint_beneficiary(us_constitution_meaning__originalist_reading, constitutional_litigators).
narrative_ontology:constraint_victim(us_constitution_meaning__originalist_reading, nonoriginalist_judicial_dissenters).
narrative_ontology:constraint_victim(us_constitution_meaning__originalist_reading, present_day_electorate).
narrative_ontology:constraint_victim(us_constitution_meaning__originalist_reading, constitutional_litigators).
narrative_ontology:constraint_vindicates(us_constitution_meaning__originalist_reading, counter_majoritarian_restraint_norm).
narrative_ontology:constraint_vindicates(us_constitution_meaning__originalist_reading, written_constitution_fixity_premise).
narrative_ontology:constraint_vindicates(us_constitution_meaning__originalist_reading, intergenerational_contract_stability).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sit on the federal bench for life, decide which constitutional claims succeed, and apply the fixed-meaning method in every adjudication; several helped articulate the method before elevation. Leaving the bench ends their influence; renouncing the method would strand their scholarly legacies and invite charges of bad faith from the coalition that elevated them.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__originalist_reading, originalist_aligned_justices, agenda_setter,
    institutional, generational, identity_locked, national).

% Presidents and senators who screen nominees for demonstrated fidelity to fixed historical meaning and reward or punish deviation through the appointment process. The doctrine is an asset they deploy when convenient and can shelve without personal loss; their commitment lasts one election cycle at a time.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__originalist_reading, confirmation_gatekeepers, agenda_setter,
    powerful, biographical, arbitrage, national).

% Built and staff the institutions — societies, journals, academic centers, nomination pipelines — that select for adherence to fixed historical meaning. They gain judicial allies, institutional funding, and public standing whenever courts honor the constraint; they can redirect effort to politics or other causes if the doctrine loses favor.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__originalist_reading, counter_majoritarian_constraint_advocates, beneficiary,
    organized, generational, mobile, national).

% Professors, historians, and method-builders who supply the ratification-era evidence the constraint consumes. Careers, citations, centers, and consultancies depend on the doctrine staying operative; abandoning it forfeits accumulated expertise and standing. They also bear its discipline: their conclusions must serve fixation or lose the coalition's platform.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__originalist_reading, originalist_scholarship_community, beneficiary,
    organized, biographical, identity_locked, national).

% Litigants seeking constitutional protection for harms or liberties whose vindication lacks founding- or amendment-era analogues. Their channel runs through the courts; when the fixed-meaning test finds no historical warrant, the door closes, and the alternative routes open to them — legislation, amendment, state law — are slow, uncertain, and often unavailable to the politically weak. The loss is timed to their lives: a protection recognized a generation late arrives after their need has passed.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__originalist_reading, rights_claimants_without_historical_support, payer,
    powerless, immediate, trapped, national).

% Groups — including descendants of the enslaved, women, and tribal nations — whose members were barred from the founding-era public whose understanding the constraint treats as authoritative. The historical record rarely registers their constitutional claims, so the test that consults only that record reproduces their original exclusion; leaving this position would require rewriting the evidentiary base itself.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__originalist_reading, communities_unrepresented_at_ratification, payer,
    powerless, generational, trapped, national).
narrative_ontology:stakeholder_secondary_role(us_constitution_meaning__originalist_reading, communities_unrepresented_at_ratification, excluded).

% Judges and scholars who read constitutional principles as carrying forward through changed circumstances. They publish dissenting opinions, train students, and argue in journals, but their preferred outcomes are screened out of nominations and their jurisprudence commands a declining institutional share; switching camps would cost them their intellectual identities and audiences.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__originalist_reading, nonoriginalist_judicial_dissenters, payer,
    moderate, generational, identity_locked, national).

% Citizens who, under this constraint, hold the levers of constitutional change: new meaning enters chiefly through amendment and ordinary politics rather than judicial revision, which concentrates agenda control with majorities. They pay diffusely when widely supported protections find no historical warrant and courts decline to recognize them.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__originalist_reading, present_day_electorate, beneficiary,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(us_constitution_meaning__originalist_reading, present_day_electorate, payer).

% Advocates who must argue cases within the fixed-meaning test: they commission archival research, corpus tools, and historian affidavits, sustaining a specialist market. They gain billable demand from the method's requirements, yet bear its costs whenever the only winning argument available to a client is one the historical record does not supply.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__originalist_reading, constitutional_litigators, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(us_constitution_meaning__originalist_reading, constitutional_litigators, payer).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(us_constitution_meaning__originalist_reading, counter_majoritarian_constraint_advocates).
narrative_ontology:fixing_cost_class(us_constitution_meaning__originalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Anchors constitutional adjudication to a publicly verifiable evidentiary standard: all parties to a constitutional dispute can anticipate that outcomes will be governed by ratification-era public meaning, which disciplines judicial discretion, stabilizes expectations across generations, and preserves a shared intergenerational text that no sitting majority or bench can quietly rewrite.
% TRANSFER_FUNCTION: Moves the authority to say what the Constitution requires away from sitting judges' contemporary moral judgment and toward the historical public record of ratification — and correspondingly moves contested rights questions from courtrooms to legislatures and the amendment process, transferring agenda control over constitutional change to elected majorities and away from litigants.
% ABSENT_VOICES: Communities excluded from the ratification-era public — enslaved people, women, Indigenous nations — whose understanding of the Constitution was never recorded and therefore never enters the evidence the constraint treats as controlling; present-day claimants whose rights concepts postdate the last relevant amendment are similarly outside the evidentiary universe. Both would object that 'public meaning' is the meaning of a restricted public; they are absent from the record itself, not merely from today's proceedings (paired with communities_unrepresented_at_ratification, roles payer/excluded).
% DISAPPEARANCE_RATIONALE: Overnight removal would reorganize adjudication immediately: courts would weigh contemporary understandings alongside text, rights jurisprudence would expand along whichever lines benches favor, confirmation politics would retool around different selection criteria, and the amendment-and-politics monopoly on constitutional change would dissolve as courts re-entered the business of updating meaning. The counter-majoritarian architecture — pipelines, centers, screening rituals — would lose its organizing purpose within a decade.
% FOUNDING_PROBLEM: Unchecked judicial discretion over constitutional meaning: the perception, sharpened by mid-twentieth-century rights revolutions, that an unelected bench could remake the fundamental law by interpretation alone, severing the Constitution from the sovereign act that authored it and converting judicial review from limit into instrument.
% FOUNDING_PROBLEM_CORROBORATION: Outside the beneficiary set, empirical political science documents substantial judicial discretion in constitutional cases, and nonoriginalist jurists themselves attest that judges exercise interpretive creativity — they celebrate rather than lament it, but they corroborate that the problem exists. Whether it remains grave enough to justify the fixation remedy is disputed by those same sources; no neutral body certifies the founding problem as either solved or unchanged, and the constraint's own beneficiaries are not relied upon for attestation.
narrative_ontology:disappearance_verdict(us_constitution_meaning__originalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(us_constitution_meaning__originalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_meaning__originalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(us_constitution_meaning__originalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(us_constitution_meaning__originalist_reading, 0.62, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_meaning__originalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_constitution_meaning__originalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(us_constitution_meaning__originalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored at 0.62 because the constraint's bite is real and concentrated — claims without historical warrants fail wholesale, and the failure rate scales with moral distance from founding-era categories — while a large share of its operation prices genuine constraint services (predictability, legitimacy, anti-usurpation discipline). Suppression is higher (0.75) than extraction because persistence depends on actively excluding rival interpretive authority from the bench rather than on participant preference: screening, promotion ladders, and citation discipline do the holding; suppression is authored as a raw structural property and the engine, not the story, scales extractiveness by directionality and scope. Theater_ratio 0.38: the historical method is genuinely functional (documentary editions, corpus projects, archival practice), but a growing share of activity is performative fidelity — curated history invoked to bless outcomes reached on other grounds. Accessibility_collapse 0.58: within a committed judge's decision frame, understanding the doctrine closes the alternative (updating meaning) almost completely, but systemic alternatives survive outside adjudication through amendment and legislation, so collapse is partial. Resistance 0.62: rival-readings jurisprudence, dissenting opinions, and the academic counter-tradition meet the constraint continuously; victim-class coalition potential is structurally thin — affected claimants are heterogeneous, losses arrive case-by-case at the chamber door, and each loses alone, which is why resistance concentrates in the professions rather than among claimants. Claim/metric independence: claimed_type tangled_rope states my structural belief — a genuine coordination function joined to asymmetric extraction held together by active enforcement — while the metrics independently describe operation; the engine computes per-seat types from the structural data and any divergence from my claim is corpus data, not error. The three measurement series share one seven-point grid so no metric row borrows another's timeline; suppression_requirement is tracked because this story specifically traces enforcement-capacity build-up (screening institutionalization after the 1987 confirmation fight, movement infrastructure maturation, method tooling), not merely extraction drift.
 *
 * PERSPECTIVAL GAP:
 *   Seats should compute differently. From the rights_claimants_without_historical_support seat the arrangement presents as a closed adjudicative door with a historical test as its lock — denial without offsetting service. From the originalist_aligned_justices seat it presents as faithful execution of a prior sovereign act — a discipline they volunteered for, are honored by, and cannot abandon without self-negation: the identity-lock here is professional and institutional (their jurisprudential legacy is the doctrine; recantation dissolves both career meaning and coalition standing). From the counter_majoritarian_constraint_advocates seat it is an achieved safeguard being defended. The scholarship community's lock is career-path dependence — decades of accumulated method-specific expertise are unredeployable. The same evidentiary rule reads as liberation or closure depending on whether the seat's claims possess historical warrants, and the engine derives that asymmetry from the beneficiary/victim declarations and exit structures rather than from my claim.
 *
 * DIRECTIONALITY LOGIC:
 *   Declarations drive derivation: rights_claimants_without_historical_support and communities_unrepresented_at_ratification sit in victims with powerless power and trapped exits — near-full-target d. counter_majoritarian_constraint_advocates and originalist_scholarship_community sit in beneficiaries with mobile or voluntarily identity-locked exits — low d, the scholarship community slightly raised by its maintenance burdens. The present_day_electorate is deliberately left out of the arrays because it is genuinely mixed (agenda-control gains, diffuse recognition losses) and should land near symmetric from fallback. One override is declared for the institutional power class, occupied in this story solely by originalist_aligned_justices: the derivation chain has no declaration for this undeclared agenda_setter seat and would likely read identity-lock plus life tenure as trapping, pushing d toward the target end; but adherence is consensual, rewarded, and ideologically embraced, placing the seat modestly on the beneficiary side at 0.35.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying as tangled_rope keeps both halves visible: the coordination half (credible intergenerational constraint) forbids a pure-extraction reading that would erase the doctrine's real service, and the extraction half (two named victim classes plus active enforcement) forbids a pure-coordination reading that would erase the concentrated costs. Mandatrophy risk sits ahead of the story, not inside it: the founding problem (judicial discretion) is contested rather than dead, so the doctrine is not vestigial; the rising theater_ratio series is the watch-item — if performative fidelity continues displacing functional historical work while enforcement machinery keeps the form alive, the structure drifts toward degraded inertia. The R5 interview feeds that monitoring rather than pre-judging it: status contested crossed with disappearance world_rearranges raises no capture-or-zombie flag, correctly reflecting a doctrine whose motivating problem persists and whose function is still performed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    originalist_reading_kernel_position,
    'This constraint is the originalist_reading of the kernel us_constitution_meaning; how would instantiating a sibling reading change the constraint''s structure?',
    'Author the sibling stories (us_constitution_meaning__living_constitutionalist_reading, us_constitution_meaning__positivist_reading) and compare victim sets, epsilon, and enforcement surfaces across the family.',
    'Under the living reading the victim set shrinks (claims assessed against evolved standards), suppression shifts from outcome-exclusion to precedent-defense, and epsilon falls; under the positivist reading authority migrates from historical evidence to enactment procedure and the beneficiary set becomes procedural-integrity defenders. The disagreement is located in the single structural element both sides contest: the criterion for judicially binding meaning (fixed at ratification versus evolved-with-application versus procedurally-derived).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(originalist_reading_kernel_position, conceptual, 'Committer-frame position of this reading within the meaning kernel.').

omega_variable(
    anchor_semantics_or_partisan_construct,
    'Is fixation of constitutional meaning a general structural feature of durable written constitutions (any long-lived constitution needs an anchor semantics to remain a constraint rather than an oracle), or a constructed doctrine serving identifiable coalitions?',
    'Comparative constitutional analysis of written-constitution regimes with and without fixation doctrines; correlate fixation adoption with measurable coalition benefit (placement yields, institutional growth, agenda control).',
    'If anchor-semantics is generic, part of measured extraction is the unavoidable price of written constitutionalism and the coordination half dominates; if constructed, extraction is coalition rent and the asymmetric half dominates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(anchor_semantics_or_partisan_construct, empirical, 'Whether meaning-fixation is a natural feature of written constitutionalism or a constructed coalition asset.').

omega_variable(
    internalized_or_structural_judge_discipline,
    'Is adherence enforced by structural machinery (confirmation screening, promotion ladders, citation networks) or internalized professional identity (judges sincerely committed to fixation)?',
    'Track post-appointment convergence: judges elevated for fidelity who later face cases where the historical record cuts against their prior commitments — meaningful defection rates indicate structural suppression; rare defection even off-camera indicates internalization dominates.',
    'If largely internalized, suppression outlives the enforcement machinery: liberalizing confirmation politics would not quickly relax the constraint, and effective suppression during any transition period would exceed the structural measure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_or_structural_judge_discipline, empirical, 'Structural versus internalized mechanism of judicial adherence.').

omega_variable(
    meaning_application_boundary_elasticity,
    'How elastic is the boundary the doctrine draws between fixed meaning and flexible application, and does the boundary migrate under pressure?',
    'Code the doctrine''s rulings and scholarly output for cases where application-flexibility absorbed what resembles meaning-change (expanded protection categories justified as ''applying'' fixed concepts to new circumstances); measure boundary migration across decades.',
    'A migrating boundary lowers effective extraction below the static estimate, since flexibility absorbs claims the fixation test nominally excludes; a hard boundary confirms the full measured bite lands on unhistorically warranted claimants.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(meaning_application_boundary_elasticity, empirical, 'Elasticity of the meaning/application boundary that modulates the constraint''s bite.').

omega_variable(
    gain_capture_by_advocacy_coalition,
    'Does the advocacy coalition capture the constraint''s proceeds (judicial placements, institutional growth, doctrinal authority) as coordinated return, or are the gains dispersed across diffuse rule-of-law value?',
    'Trace resource flows around the doctrine: nomination-pipeline yields, center and journal funding trajectories, consultancy markets, against counterfactual dispersion models.',
    'Confirmed capture supports the named receipt seat and keeps the asymmetric-extraction reading dominant; dispersed gains would move the receipt surface toward diffuse and soften extraction toward coordination-cost pricing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gain_capture_by_advocacy_coalition, empirical, 'Whether the constraint''s gains accrue to a capturable seat or diffuse.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_meaning__originalist_reading, 1971, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_c_tr_t1971, us_constitution_meaning__originalist_reading, theater_ratio, 1971, 0.15).
narrative_ontology:measurement_basis(us_c_tr_t1971, observed).
narrative_ontology:measurement(us_c_tr_t1981, us_constitution_meaning__originalist_reading, theater_ratio, 1981, 0.18).
narrative_ontology:measurement_basis(us_c_tr_t1981, observed).
narrative_ontology:measurement(us_c_tr_t1991, us_constitution_meaning__originalist_reading, theater_ratio, 1991, 0.23).
narrative_ontology:measurement_basis(us_c_tr_t1991, observed).
narrative_ontology:measurement(us_c_tr_t2001, us_constitution_meaning__originalist_reading, theater_ratio, 2001, 0.27).
narrative_ontology:measurement_basis(us_c_tr_t2001, observed).
narrative_ontology:measurement(us_c_tr_t2011, us_constitution_meaning__originalist_reading, theater_ratio, 2011, 0.31).
narrative_ontology:measurement_basis(us_c_tr_t2011, observed).
narrative_ontology:measurement(us_c_tr_t2021, us_constitution_meaning__originalist_reading, theater_ratio, 2021, 0.35).
narrative_ontology:measurement_basis(us_c_tr_t2021, observed).
narrative_ontology:measurement(us_c_tr_t2025, us_constitution_meaning__originalist_reading, theater_ratio, 2025, 0.38).
narrative_ontology:measurement_basis(us_c_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(us_c_be_t1971, us_constitution_meaning__originalist_reading, base_extractiveness, 1971, 0.3).
narrative_ontology:measurement_basis(us_c_be_t1971, observed).
narrative_ontology:measurement(us_c_be_t1981, us_constitution_meaning__originalist_reading, base_extractiveness, 1981, 0.4).
narrative_ontology:measurement_basis(us_c_be_t1981, observed).
narrative_ontology:measurement(us_c_be_t1991, us_constitution_meaning__originalist_reading, base_extractiveness, 1991, 0.47).
narrative_ontology:measurement_basis(us_c_be_t1991, observed).
narrative_ontology:measurement(us_c_be_t2001, us_constitution_meaning__originalist_reading, base_extractiveness, 2001, 0.53).
narrative_ontology:measurement_basis(us_c_be_t2001, observed).
narrative_ontology:measurement(us_c_be_t2011, us_constitution_meaning__originalist_reading, base_extractiveness, 2011, 0.57).
narrative_ontology:measurement_basis(us_c_be_t2011, observed).
narrative_ontology:measurement(us_c_be_t2021, us_constitution_meaning__originalist_reading, base_extractiveness, 2021, 0.6).
narrative_ontology:measurement_basis(us_c_be_t2021, observed).
narrative_ontology:measurement(us_c_be_t2025, us_constitution_meaning__originalist_reading, base_extractiveness, 2025, 0.62).
narrative_ontology:measurement_basis(us_c_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(us_c_su_t1971, us_constitution_meaning__originalist_reading, suppression_requirement, 1971, 0.45).
narrative_ontology:measurement_basis(us_c_su_t1971, observed).
narrative_ontology:measurement(us_c_su_t1981, us_constitution_meaning__originalist_reading, suppression_requirement, 1981, 0.52).
narrative_ontology:measurement_basis(us_c_su_t1981, observed).
narrative_ontology:measurement(us_c_su_t1991, us_constitution_meaning__originalist_reading, suppression_requirement, 1991, 0.6).
narrative_ontology:measurement_basis(us_c_su_t1991, observed).
narrative_ontology:measurement(us_c_su_t2001, us_constitution_meaning__originalist_reading, suppression_requirement, 2001, 0.66).
narrative_ontology:measurement_basis(us_c_su_t2001, observed).
narrative_ontology:measurement(us_c_su_t2011, us_constitution_meaning__originalist_reading, suppression_requirement, 2011, 0.7).
narrative_ontology:measurement_basis(us_c_su_t2011, observed).
narrative_ontology:measurement(us_c_su_t2021, us_constitution_meaning__originalist_reading, suppression_requirement, 2021, 0.74).
narrative_ontology:measurement_basis(us_c_su_t2021, observed).
narrative_ontology:measurement(us_c_su_t2025, us_constitution_meaning__originalist_reading, suppression_requirement, 2025, 0.75).
narrative_ontology:measurement_basis(us_c_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_meaning__originalist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(us_constitution_meaning__originalist_reading, us_constitution_meaning__living_constitutionalist_reading).
narrative_ontology:affects_constraint(us_constitution_meaning__originalist_reading, us_constitution_meaning__positivist_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'what the Constitution means' decomposes into three structurally distinct constraints, one per reading of the kernel us_constitution_meaning. Each member carries its own epsilon, its own beneficiary/victim sets, and its own enforcement surface; this member authors epsilon for the standing arrangement in which ratification-era public meaning controls adjudication, assessed with its victims named. Members link via affects_constraints per the family rule (sibling IDs assumed under the same naming convention). Upstream-downstream structure: the fixation premise supplies the interpretive convention that the sibling readings modify or reject, so this story sits upstream of the living reading's modification and parallel to the positivist reading's procedural re-grounding.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(us_constitution_meaning__originalist_reading, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
