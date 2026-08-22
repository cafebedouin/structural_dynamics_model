% ============================================================================
% CONSTRAINT STORY: end_of_life_decision_authority__autonomy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_end_of_life_decision_authority__autonomy_reading, []).

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
 *   constraint_id: end_of_life_decision_authority__autonomy_reading
 *   human_readable: End-of-Life Decision Authority — Autonomy Reading: the Standing Restrictive Gatekeeping Arrangement
 *   domain: bioethics/legal-medical
 *
 * SUMMARY:
 *   This story instantiates the AUTONOMY READING of the
 *   end_of_life_decision_authority kernel: the claim that competent
 *   individuals possess sovereign authority over their own death. Per the
 *   fixed epsilon-referent rule for kernel readings, epsilon is authored for
 *   the STANDING ARRANGEMENT UNDER CONTEST — the restrictive gatekeeping
 *   regime in which death-decision authority is withheld from individuals and
 *   held by legislatures, courts, licensing bodies, and treating physicians —
 *   assessed by this reading's own lights. The reading's endorsed alternative
 *   (a permissive access regime) is NOT the referent and contributes nothing
 *   to epsilon. Assessed from the autonomy seat, the standing arrangement
 *   takes competent dying people's remaining time, bodily control, and manner
 *   of death without their consent, enforces the taking through criminal and
 *   professional discipline, and leaves exits that only wealth and mobility
 *   can purchase. The claim/metric independence rule is observed:
 *   claimed_type records my independent structural judgment (tangled_rope —
 *   the arrangement genuinely screens for coercion AND asymmetrically burdens
 *   a definable class), while the metrics record what the arrangement's
 *   operation looks like from this reading. Sibling readings
 *   (sanctity_reading, vulnerability_protection_reading) are separate
 *   constraints in the same family, linked via network.affects_constraints;
 *   the contest between readings is carried in omegas, not folded into this
 *   file.
 *
 * KEY AGENTS:
 *   - suffering_prolonged_competent_adults: primary target (powerless/trapped) — bears the arrangement's full burden; no affordable exit
 *   - jurisdictional_medical_refugees: partial-exit target subset (moderate/constrained) — buys the denied outcome abroad at heavy cost
 *   - assistance_seeking_physicians: enforcement interface and collateral bearer (institutional/constrained) — administers the rules at the bedside while exposed to discipline
 *   - medical_licensing_bodies: agenda setter (institutional/arbitrage) — converts the prohibition into standing jurisdiction and budget
 *   - legislatures_and_apex_courts: agenda setter (institutional/arbitrage) — fixes the settlement, revisits only under pressure
 *   - religious_institutions: primary beneficiary (organized/arbitrage) — collects public enforcement of doctrine at zero administrative cost
 *   - palliative_care_establishment: beneficiary (organized/mobile) — receives the entire end-of-life mandate and funding stream
 *   - disability_advocacy_organizations: beneficiary (organized/mobile) — relies on the checkpoints as protection for their constituency
 *   - bioethics_commissions: analytical observer (institutional/analytical) — sees the full structure, holds no enforcement power
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(end_of_life_decision_authority__autonomy_reading, 0.78).
domain_priors:suppression_score(end_of_life_decision_authority__autonomy_reading, 0.75).
domain_priors:theater_ratio(end_of_life_decision_authority__autonomy_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(end_of_life_decision_authority__autonomy_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(end_of_life_decision_authority__autonomy_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(end_of_life_decision_authority__autonomy_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(end_of_life_decision_authority__autonomy_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(end_of_life_decision_authority__autonomy_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(end_of_life_decision_authority__autonomy_reading, tangled_rope).
narrative_ontology:human_readable(end_of_life_decision_authority__autonomy_reading, "End-of-Life Decision Authority — Autonomy Reading: the Standing Restrictive Gatekeeping Arrangement").
narrative_ontology:topic_domain(end_of_life_decision_authority__autonomy_reading, "bioethics/legal-medical").

domain_priors:requires_active_enforcement(end_of_life_decision_authority__autonomy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(end_of_life_decision_authority__autonomy_reading, 'cb927cc0-0b06-4e21-ae00-220cd4ab196d').
narrative_ontology:cs_kernel_codification('cb927cc0-0b06-4e21-ae00-220cd4ab196d', formalized).
narrative_ontology:cs_authority_grounding('cb927cc0-0b06-4e21-ae00-220cd4ab196d', lineage).
narrative_ontology:cs_interpretation_layer_present('cb927cc0-0b06-4e21-ae00-220cd4ab196d').
narrative_ontology:cs_reading_relation('cb927cc0-0b06-4e21-ae00-220cd4ab196d', end_of_life_decision_authority__sanctity_reading, coexists_with).
narrative_ontology:cs_reading_relation('cb927cc0-0b06-4e21-ae00-220cd4ab196d', end_of_life_decision_authority__vulnerability_protection_reading, influences).
narrative_ontology:cs_axiom('cb927cc0-0b06-4e21-ae00-220cd4ab196d', foundational, competent_persons_hold_sovereign_death_authority).
narrative_ontology:cs_axiom_status(competent_persons_hold_sovereign_death_authority, holdable).
narrative_ontology:cs_axiom_grounding('cb927cc0-0b06-4e21-ae00-220cd4ab196d', competent_persons_hold_sovereign_death_authority, deontological).
narrative_ontology:cs_axiom('cb927cc0-0b06-4e21-ae00-220cd4ab196d', secondary, prohibition_imposes_net_harm_on_competent_dying).
narrative_ontology:cs_axiom_status(prohibition_imposes_net_harm_on_competent_dying, holdable).
narrative_ontology:cs_axiom_grounding('cb927cc0-0b06-4e21-ae00-220cd4ab196d', prohibition_imposes_net_harm_on_competent_dying, instrumental).
narrative_ontology:cs_reference_frame('cb927cc0-0b06-4e21-ae00-220cd4ab196d', individual_self_authorship_over_dying).
narrative_ontology:cs_drift_state('cb927cc0-0b06-4e21-ae00-220cd4ab196d', contemporary_partial_liberalization, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('cb927cc0-0b06-4e21-ae00-220cd4ab196d', '').
narrative_ontology:cs_kernel_id(end_of_life_decision_authority__autonomy_reading, end_of_life_decision_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(end_of_life_decision_authority__autonomy_reading, religious_institutions).
narrative_ontology:constraint_beneficiary(end_of_life_decision_authority__autonomy_reading, medical_licensing_bodies).
narrative_ontology:constraint_beneficiary(end_of_life_decision_authority__autonomy_reading, palliative_care_establishment).
narrative_ontology:constraint_beneficiary(end_of_life_decision_authority__autonomy_reading, disability_advocacy_organizations).
narrative_ontology:constraint_victim(end_of_life_decision_authority__autonomy_reading, suffering_prolonged_competent_adults).
narrative_ontology:constraint_victim(end_of_life_decision_authority__autonomy_reading, assistance_seeking_physicians).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(end_of_life_decision_authority__autonomy_reading, jurisdictional_medical_refugees).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Competent adults with terminal or degenerative conditions who judge their remaining trajectory worse than death and petition for assistance in dying. Requests are refused or rerouted to palliation they may find inadequate. Their remaining time, bodily control, and manner of death are governed by others. Exit means violent unsupervised attempts that risk failure and trauma, or nothing at all; travel abroad is unavailable to the immobile, the poor, and the rapidly deteriorating.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__autonomy_reading, suffering_prolonged_competent_adults, payer,
    powerless, immediate, trapped, global).

% The minority of applicants with money, mobility, and enough time to travel to permissive jurisdictions such as Swiss clinics. They obtain the outcome the arrangement denies at home, but at heavy cost: tens of thousands in fees, premature travel while still fit enough to move, dying far from family, and posthumous legal exposure for anyone who assists. Their partial exit marks the gradient the trapped majority below them cannot use.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__autonomy_reading, jurisdictional_medical_refugees, payer,
    moderate, biographical, constrained, continental).

% Physicians who receive end-of-life requests at the bedside. Refusal is default compliance; granting a request exposes license and liberty. They operate the arrangement daily — assessing, deflecting, documenting — while carrying moral distress when they believe a request is settled and rational, and prosecution or discipline risk when they act on that belief. Their professional formation binds them to the arrangement's terms even when their judgment conflicts with it.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__autonomy_reading, assistance_seeking_physicians, payer,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(end_of_life_decision_authority__autonomy_reading, assistance_seeking_physicians, agenda_setter).

% Issue guidance defining assistance in dying as professional misconduct, investigate complaints, discipline members, and publish interpretive bulletins on end-of-life conduct. The prohibition converts into standing jurisdiction, budget, and gatekeeping centrality over the profession's most consequential decisions. Nothing about their position requires them to bear the clinical costs of the rules they administer.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__autonomy_reading, medical_licensing_bodies, agenda_setter,
    institutional, generational, arbitrage, national).

% Enact and uphold the criminal prohibitions and constitutional interpretations that fix death-decision authority away from individuals. Revisit the settlement only under sustained litigation, referendum, or parliamentary pressure, and are electorally exposed to the organized coalitions that defend it. Individual members can exit the issue; the institution cannot.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__autonomy_reading, legislatures_and_apex_courts, agenda_setter,
    institutional, generational, arbitrage, national).

% Secure public enforcement of doctrinal claims about the meaning and limits of human death without administering the system that enforces them. Mobilize voters, fund litigation, and supply the moral vocabulary in which the prohibitions are defended. They bear none of the clinical or carceral cost of the arrangement and lose nothing material if it persists indefinitely.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__autonomy_reading, religious_institutions, beneficiary,
    organized, civilizational, arbitrage, global).

% Receive the entire mandate and funding stream for end-of-life suffering on the arrangement's terms: every request for assistance is converted into a referral for palliation. Genuine service provision is entangled with a monopoly position; access expansion would divide the mandate, so the sector's institutional leadership opposes it even where individual clinicians are sympathetic.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__autonomy_reading, palliative_care_establishment, beneficiary,
    organized, biographical, mobile, national).

% Rely on the checkpoint regime as protection against subtle economic and relational pressure on disabled people, and genuinely fear that access expansion converts despair into eligibility. Their constituency overlaps the applicant pool: the same people they protect are people the arrangement confines. They advocate from outside the clinical encounter and can continue advocating under any settlement.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__autonomy_reading, disability_advocacy_organizations, beneficiary,
    organized, generational, mobile, national).

% Convene hearings, commission evidence from permissive jurisdictions, and publish reports weighing individual determination against protection of the vulnerable. Hold no enforcement power; their analyses feed legislative committees and court opinions and are cited by every side of the dispute.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__autonomy_reading, bioethics_commissions, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(end_of_life_decision_authority__autonomy_reading, medical_licensing_bodies).
narrative_ontology:fixing_cost_class(end_of_life_decision_authority__autonomy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The arrangement solves a real screening problem: distinguishing authentic, settled, competent requests for death from requests produced by coercion, untreated depression, family pressure, or reversible despair, and it maintains a unified medical ethos in which patients need not suspect their physician of an interest in their death.
% TRANSFER_FUNCTION: Moves decision authority over the timing and manner of death from competent dying individuals up to legislatures, courts, licensing bodies, and treating physicians; moves the burden of prolonged unwanted dying onto the individuals themselves; and moves the risk of unsupervised self-harm from the regulated sphere onto families and coroners.
% ABSENT_VOICES: The competent suffering adults are formally present in consultations but structurally voiceless: their testimony of settled intent is routinely re-read as depression, delirium, or manipulation, so the people the arrangement most directly governs rarely appear as testifying principals in the forums that set it. The poor and immobile, who cannot buy exit through travel, are absent entirely; future patients are represented only by advocacy proxies on both sides.
% DISAPPEARANCE_RATIONALE: If the prohibitions and their enforcement machinery vanished overnight, physicians could respond to requests immediately, prosecutions and disciplinary dockets would empty, the palliative mandate would divide, hospice funding formulas would be renegotiated, and thousands of deaths per year would be re-timed and re-sited. The screening function the arrangement performs would need rapid reconstruction in whatever replaced it — the world does not stay put.
% FOUNDING_PROBLEM: The arrangement was built to prevent coercion and abuse of vulnerable people at the end of life, to preserve public trust that physicians heal rather than kill, and to reconcile criminal law with medical ethics after the historical abuses of officially sanctioned euthanasia programs.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: bioethics commission reports and the comparative empirical literature from permissive jurisdictions document that coercion-screening remains a live operational problem wherever assistance is legal; the autonomy movement's own insistence on eligibility thresholds and safeguards is tacit corroboration that the underlying hazard is real. Religious institutions also attest the problem is live, but they sit inside the beneficiary set, so their attestation carries no independent weight here.
narrative_ontology:disappearance_verdict(end_of_life_decision_authority__autonomy_reading, world_rearranges).
narrative_ontology:founding_problem_status(end_of_life_decision_authority__autonomy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(end_of_life_decision_authority__autonomy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(end_of_life_decision_authority__autonomy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(end_of_life_decision_authority__autonomy_reading, 0.78, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(end_of_life_decision_authority__autonomy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(end_of_life_decision_authority__autonomy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(end_of_life_decision_authority__autonomy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78 at interval end) because the arrangement withholds a decision the affected individuals are competent to make, and the burden — prolonged unwanted dying — falls entirely on them while the deciding is done by others. Suppression is high (0.75) because persistence depends on active machinery: criminal statutes, licensing discipline, and the professional formation that makes refusal the default; suppression is authored as a raw structural property and is not scaled by scope or power — only extractiveness is scaled downstream. Theater rises to 0.42 because, as liberalization spreads across jurisdictions, the holdout arrangement's ethics-committee rituals, suicide-prevention framing, and palliative-mandate rhetoric increasingly perform a screening function that the comparative evidence shows can be done by explicit eligibility procedures; the functional core (coercion screening) remains real, which is why theater stays below the piton range. Accessibility_collapse is 0.65: once a competent applicant understands the refusal, alternatives collapse to violent unsupervised attempts, inadequate palliation, or costly foreign travel — collapsed but not completely, since the travel route exists. Resistance is 0.60 and rising: litigation, referenda, physician civil disobedience, and organized advocacy meet the arrangement continuously. The three measurement series share one eight-point grid (t=0..35, roughly 1990–2025) so every metric is authored at every examined time point; all three rise monotonically, reflecting aging populations with longer dying phases, enforcement machinery that matured (explicit statutes, discipline guidelines, telemedicine bans) even as some jurisdictions liberalized, and accumulating burden on the trapped class. The rising base_extractiveness series is the accumulation signature the temporal-abduction trigger watches; it does not reclassify anything here, it flags the trend for investigation. No cyclical dynamics are authored: the drift is monotonic, not oscillatory.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the agenda-setter seats (licensing bodies, legislatures), the arrangement is a legitimate protective settlement they administer; from the payer seats (competent suffering adults, refugees, exposed physicians), the identical structure operates as enforced confiscation of a decision that is theirs. The sharpest divergence is disability_advocacy_organizations: a beneficiary seat whose members' interests overlap the victim class — the same arrangement that protects their constituency from pressure confines the competent among them. Coalition potential among the powerless is real but historically weak: the trapped are dispersed, dying, and organizationally exhausted, so their resistance has been carried almost entirely by proxy organizations and sympathetic physicians rather than by the affected class itself — which is also why the arrangement's resistance metric reflects proxy litigation more than direct refusal capacity.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary and victim declarations drive the derivation. suffering_prolonged_competent_adults and jurisdictional_medical_refugees sit at the full-target end (trapped or expensive-constrained exit amplifies their effective burden; the refugee seat's purchasable exit moderates but does not invert it). assistance_seeking_physicians derive mid-to-high: they enforce the arrangement (pushing toward beneficiary) while bearing its discipline risk and moral cost (pulling toward target) — the dual role is authored structurally rather than overridden, because the derivation from their dual declaration already lands them near symmetric. religious_institutions, palliative_care_establishment, and disability_advocacy_organizations sit near the beneficiary end: each collects something durable from the arrangement's persistence and bears little of its operating cost. medical_licensing_bodies and legislatures_and_apex_courts are agenda setters whose collected rents are jurisdictional rather than pecuniary; their d sits low but not at zero, since administering the arrangement also exposes them to legitimacy challenge. No directionality_overrides are used: the beneficiary/victim declarations plus exit grades produce the correct relationships, and an override keyed to a power atom would misfire across the multiple institutional seats at the same power level.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope claim is what keeps this story from being mislabeled in either direction. Reading it as pure extraction would erase the genuine coordination function — coercion screening and medical-trust maintenance are real problems that any successor arrangement must solve, and the founding problem is corroborated as live from outside the beneficiary set. Reading it as pure coordination would erase the identifiable class that pays for the settlement with their remaining lives. Mandatrophy is NOT resolved: the founding problem (coercion screening) is live, so the arrangement has not outlived its function wholesale. What HAS atrophied is the proportion of activity that serves the function versus the proportion that defends the settlement — visible in the rising theater_ratio series — which is the early signature of a rope decaying toward piton in holdout jurisdictions if liberalization pressure ever ceases. The mismatch consumer reads founding_problem_status=live against disappearance_verdict=world_rearranges and finds no zombie flag: the arrangement persists because its problem persists, not because its problem vanished.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_allocation,
    'Which reading of the end_of_life_decision_authority kernel should govern — and what would each sibling change structurally if adopted?',
    'Not resolvable by data alone: the allocation is a normative commitment. Comparative jurisdiction outcomes can inform but not settle it; the resolution mechanism is continued constitutional and legislative contest in which each reading''s victim set and burden distribution is made explicit.',
    'Under sanctity_reading the applicant class loses all entitlement (victim set empties, epsilon referent shifts to the permissive arrangements themselves); under vulnerability_protection_reading competent applicants become screened petitioners and the checkpoint apparatus becomes the constraint rather than its alternative. This story''s classification holds only within the autonomy reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_allocation, conceptual, 'Committer-frame omega: this constraint is the autonomy_reading of a three-reading kernel; sibling readings instantiate different constraints with different victim sets.').

omega_variable(
    competence_threshold_location,
    'Where exactly does decision-making competence sit — and does the line track a real capacity boundary or an institutional convenience?',
    'Longitudinal capacity-assessment studies comparing assessed competence at request time with later-verified settled intent, across jurisdictions with different threshold instruments.',
    'A stricter-than-necessary threshold shrinks the entitled class and inflates measured extraction on the excluded; a looser threshold admits pressured applicants and strengthens the sibling readings'' case. The victim set boundary and the reading''s internal coherence both move with the line.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(competence_threshold_location, conceptual, 'The autonomy reading''s load-bearing ambiguity: competence is doing the work of the whole reading, and its location is contestable.').

omega_variable(
    coercion_screening_displacement,
    'Does the standing arrangement actually screen coercion effectively, or does it mainly displace assisted dying into unregulated, unscreened self-harm?',
    'Comparative jurisdiction data: unassisted suicide rates, violent-method incidence, and documented coercion episodes before and after regulated access, controlling for palliative-care availability.',
    'If displacement dominates, the arrangement''s coordination claim collapses toward pure extraction sustained by enforcement — the tangled_rope claim fails and the snare reading of the same structure gains force. If screening works, the coordination half of the claim stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coercion_screening_displacement, empirical, 'Whether the arrangement''s genuine coordination function is real screening or cover for displacement.').

omega_variable(
    slippery_slope_incidence,
    'How much of the feared slope risk is real, and who bears it — does access expansion produce measurable pressure on disabled, depressed, or economically burdened people to choose death?',
    'Population-level studies from long-running permissive jurisdictions tracking eligibility creep, safeguard-bypass rates, and coercion indicators among non-terminal and disabled populations.',
    'Material slope incidence strengthens vulnerability_protection_reading and forces this reading to internalize the risk it currently externalizes; negligible incidence weakens the checkpoint apparatus''s justification and supports the autonomy reading''s competence-line solution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(slippery_slope_incidence, empirical, 'The externalized risk this reading pushes onto the sibling readings: its resolution reallocates burden between the readings.').

omega_variable(
    wealth_exit_gradient,
    'How much of the arrangement''s burden varies with wealth and mobility — is the effective denial experienced equally across the applicant class?',
    'Demographic audit of who obtains foreign-territory assistance versus who dies unsupervised at home: income, mobility, and diagnosis distributions across the two outcomes.',
    'A steep gradient concentrates the arrangement''s effective burden on the poor and immobile, widening computed divergence between the refugee seat and the trapped seat and supporting equity-targeted remedies; a flat gradient simplifies the victim class into a single seat.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(wealth_exit_gradient, empirical, 'Exit heterogeneity inside the victim class drives per-seat divergence in computed burden.').

omega_variable(
    beneficiary_capture_migration,
    'As religious affiliation declines, does the arrangement''s sustaining coalition migrate from doctrinal enforcement toward licensing-body jurisdiction and palliative-sector mandate — and does the seat that collects the gains move with it?',
    'Track the composition of legislative and litigation defense coalitions over successive reform cycles: which beneficiary seats supply the votes, funds, and expert testimony.',
    'If capture migrates to administrative and sectoral seats, the arrangement''s persistence becomes increasingly inertial-administrative rather than doctrinal, shifting the theater trajectory upward and moving the piton-decay scenario closer; the identified receiver of gains would shift seats accordingly.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_capture_migration, conceptual, 'Whether the beneficiary structure underneath the arrangement is stable or migrating across seats.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(end_of_life_decision_authority__autonomy_reading, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(end__tr_t0, end_of_life_decision_authority__autonomy_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(end__tr_t5, end_of_life_decision_authority__autonomy_reading, theater_ratio, 5, 0.24).
narrative_ontology:measurement(end__tr_t10, end_of_life_decision_authority__autonomy_reading, theater_ratio, 10, 0.27).
narrative_ontology:measurement(end__tr_t15, end_of_life_decision_authority__autonomy_reading, theater_ratio, 15, 0.3).
narrative_ontology:measurement(end__tr_t20, end_of_life_decision_authority__autonomy_reading, theater_ratio, 20, 0.33).
narrative_ontology:measurement(end__tr_t25, end_of_life_decision_authority__autonomy_reading, theater_ratio, 25, 0.36).
narrative_ontology:measurement(end__tr_t30, end_of_life_decision_authority__autonomy_reading, theater_ratio, 30, 0.39).
narrative_ontology:measurement(end__tr_t35, end_of_life_decision_authority__autonomy_reading, theater_ratio, 35, 0.42).

% Extraction over time
narrative_ontology:measurement(end__be_t0, end_of_life_decision_authority__autonomy_reading, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(end__be_t5, end_of_life_decision_authority__autonomy_reading, base_extractiveness, 5, 0.63).
narrative_ontology:measurement(end__be_t10, end_of_life_decision_authority__autonomy_reading, base_extractiveness, 10, 0.66).
narrative_ontology:measurement(end__be_t15, end_of_life_decision_authority__autonomy_reading, base_extractiveness, 15, 0.69).
narrative_ontology:measurement(end__be_t20, end_of_life_decision_authority__autonomy_reading, base_extractiveness, 20, 0.72).
narrative_ontology:measurement(end__be_t25, end_of_life_decision_authority__autonomy_reading, base_extractiveness, 25, 0.74).
narrative_ontology:measurement(end__be_t30, end_of_life_decision_authority__autonomy_reading, base_extractiveness, 30, 0.76).
narrative_ontology:measurement(end__be_t35, end_of_life_decision_authority__autonomy_reading, base_extractiveness, 35, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(end__su_t0, end_of_life_decision_authority__autonomy_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(end__su_t5, end_of_life_decision_authority__autonomy_reading, suppression_requirement, 5, 0.58).
narrative_ontology:measurement(end__su_t10, end_of_life_decision_authority__autonomy_reading, suppression_requirement, 10, 0.61).
narrative_ontology:measurement(end__su_t15, end_of_life_decision_authority__autonomy_reading, suppression_requirement, 15, 0.64).
narrative_ontology:measurement(end__su_t20, end_of_life_decision_authority__autonomy_reading, suppression_requirement, 20, 0.67).
narrative_ontology:measurement(end__su_t25, end_of_life_decision_authority__autonomy_reading, suppression_requirement, 25, 0.7).
narrative_ontology:measurement(end__su_t30, end_of_life_decision_authority__autonomy_reading, suppression_requirement, 30, 0.73).
narrative_ontology:measurement(end__su_t35, end_of_life_decision_authority__autonomy_reading, suppression_requirement, 35, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(end_of_life_decision_authority__autonomy_reading, identity_coordination).
narrative_ontology:affects_constraint(end_of_life_decision_authority__autonomy_reading, end_of_life_decision_authority__sanctity_reading).
narrative_ontology:affects_constraint(end_of_life_decision_authority__autonomy_reading, end_of_life_decision_authority__vulnerability_protection_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the colloquial label 'end-of-life decision authority'. The single natural-language concept covers three structurally distinct constraints — autonomy_reading (this file: individual sovereignty; victim set = competent applicants denied access; high epsilon for the standing restrictive arrangement), sanctity_reading (will-independent value of life; no applicant entitlement; epsilon attaches to permissive arrangements where they exist), and vulnerability_protection_reading (distributed checkpoint authority; applicants become screened petitioners). Each member carries its own epsilon, beneficiaries, victims, and claimed_type; the family is linked via network.affects_constraints in all three files. Upstream/downstream: sanctity_reading is the historically upstream claim (its doctrine supplied the founding vocabulary of the standing arrangement) and structurally influences this reading's operating environment; this reading's spread exerts downstream pressure on vulnerability_protection_reading by forcing checkpoints to justify themselves as safeguards rather than authority-holders.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
