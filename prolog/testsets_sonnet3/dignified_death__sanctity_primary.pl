% ============================================================================
% CONSTRAINT STORY: dignified_death__sanctity_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dignified_death__sanctity_primary, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: dignified_death__sanctity_primary
 *   human_readable: Sanctity-of-Life Prohibition on Assisted Dying
 *   domain: bioethics/medical_law/political_philosophy
 *
 * SUMMARY:
 *   This story instantiates the sanctity_primary reading of the
 *   dignified_death kernel: dignity resides in life's intrinsic, transcendent
 *   value, and intentional life-termination is categorically impermissible
 *   regardless of the individual's documented, competent consent. Under this
 *   reading, the standing arrangement is the legal prohibition (partial or
 *   total) on assisted dying that the reading defends. From the
 *   sanctity-primary seat, the coordination story is protection of vulnerable
 *   populations from coercion, devaluation, and a slippery slope toward
 *   normalized killing. But authored from the same reading's own metrics, the
 *   arrangement functions substantially as a snare: the beneficiary class
 *   (religious institutions, absolutist advocacy networks, the invoked 'moral
 *   order') is structurally distinct from and largely insulated from the
 *   victim class (patients enduring unrelievable suffering with no legal
 *   exit, disproportionately concentrated among the poor, the elderly, and
 *   the disabled who cannot travel to more permissive jurisdictions). The
 *   'protection' function inverts: those it claims to protect (elderly,
 *   disabled, poor) are also disproportionately those who suffer its costs,
 *   because they lack the private means to buy either high-quality palliative
 *   alternatives or cross-border legal exit that wealthier autonomy-seeking
 *   patients can sometimes access. This is the ε-invariant referent: the
 *   standing prohibition as the sanctity-primary reading itself describes it,
 *   not the autonomy-based alternative it opposes.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dignified_death__sanctity_primary, 0.58).
domain_priors:suppression_score(dignified_death__sanctity_primary, 0.71).
domain_priors:theater_ratio(dignified_death__sanctity_primary, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dignified_death__sanctity_primary, extractiveness, 0.58).
narrative_ontology:constraint_metric(dignified_death__sanctity_primary, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(dignified_death__sanctity_primary, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dignified_death__sanctity_primary, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(dignified_death__sanctity_primary, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dignified_death__sanctity_primary, snare).
narrative_ontology:human_readable(dignified_death__sanctity_primary, "Sanctity-of-Life Prohibition on Assisted Dying").
narrative_ontology:topic_domain(dignified_death__sanctity_primary, "bioethics/medical_law/political_philosophy").

domain_priors:requires_active_enforcement(dignified_death__sanctity_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dignified_death__sanctity_primary, '6580da14-2b3f-4ff1-b40e-b3eb03c6c473').
narrative_ontology:cs_kernel_codification('6580da14-2b3f-4ff1-b40e-b3eb03c6c473', distributed).
narrative_ontology:cs_authority_grounding('6580da14-2b3f-4ff1-b40e-b3eb03c6c473', lineage).
narrative_ontology:cs_interpretation_layer_present('6580da14-2b3f-4ff1-b40e-b3eb03c6c473').
narrative_ontology:cs_reading_relation('6580da14-2b3f-4ff1-b40e-b3eb03c6c473', dignified_death__autonomy_primary, forecloses).
narrative_ontology:cs_reading_relation('6580da14-2b3f-4ff1-b40e-b3eb03c6c473', dignified_death__relational_autonomy, influences).
narrative_ontology:cs_axiom('6580da14-2b3f-4ff1-b40e-b3eb03c6c473', foundational, consent_categorically_irrelevant_to_permissibility).
narrative_ontology:cs_axiom_status(consent_categorically_irrelevant_to_permissibility, holdable).
narrative_ontology:cs_axiom_grounding('6580da14-2b3f-4ff1-b40e-b3eb03c6c473', consent_categorically_irrelevant_to_permissibility, deontological).
narrative_ontology:cs_axiom('6580da14-2b3f-4ff1-b40e-b3eb03c6c473', foundational, life_value_is_transcendent_and_non_waivable).
narrative_ontology:cs_axiom_status(life_value_is_transcendent_and_non_waivable, holdable).
narrative_ontology:cs_axiom_grounding('6580da14-2b3f-4ff1-b40e-b3eb03c6c473', life_value_is_transcendent_and_non_waivable, theological).
narrative_ontology:cs_axiom('6580da14-2b3f-4ff1-b40e-b3eb03c6c473', secondary, slippery_slope_to_coerced_death_is_near_certain).
narrative_ontology:cs_axiom_status(slippery_slope_to_coerced_death_is_near_certain, holdable).
narrative_ontology:cs_axiom_grounding('6580da14-2b3f-4ff1-b40e-b3eb03c6c473', slippery_slope_to_coerced_death_is_near_certain, empirically_contingent).
narrative_ontology:cs_reference_frame('6580da14-2b3f-4ff1-b40e-b3eb03c6c473', transcendent_natural_law_sanctity).
narrative_ontology:cs_drift_state('6580da14-2b3f-4ff1-b40e-b3eb03c6c473', post_legalization_wave_2020s, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('6580da14-2b3f-4ff1-b40e-b3eb03c6c473', '').
narrative_ontology:cs_kernel_id(dignified_death__sanctity_primary, dignified_death).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dignified_death__sanctity_primary, religious_institutions).
narrative_ontology:constraint_beneficiary(dignified_death__sanctity_primary, disability_rights_absolutist_organizations).
narrative_ontology:constraint_beneficiary(dignified_death__sanctity_primary, prolife_advocacy_networks).
narrative_ontology:constraint_beneficiary(dignified_death__sanctity_primary, moral_order_of_the_community).
narrative_ontology:constraint_victim(dignified_death__sanctity_primary, terminally_ill_patients_seeking_death).
narrative_ontology:constraint_victim(dignified_death__sanctity_primary, elderly_dependents_under_family_pressure).
narrative_ontology:constraint_victim(dignified_death__sanctity_primary, low_income_patients_without_palliative_access).
narrative_ontology:constraint_victim(dignified_death__sanctity_primary, disabled_people_denied_agency_over_own_death).
narrative_ontology:constraint_victim(dignified_death__sanctity_primary, chronic_pain_sufferers_without_legal_recourse).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(dignified_death__sanctity_primary, palliative_and_hospice_clinicians).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Lobbies legislatures, funds litigation, and shapes clinical ethics boards to keep intentional life-termination criminalized or heavily restricted regardless of patient consent. Frames the prohibition as protecting the moral order and the sanctity of life; suffers no direct cost from the prohibition's operation and gains institutional authority and moral standing from being the arrangement's designated interpreter.
narrative_ontology:constraint_stakeholder(dignified_death__sanctity_primary, religious_institutions, agenda_setter,
    institutional, civilizational, analytical, national).
narrative_ontology:stakeholder_secondary_role(dignified_death__sanctity_primary, religious_institutions, beneficiary).

% Advocates against legalization on grounds that legal assisted dying pressures disabled people toward death via cost, ableist devaluation, or family burden framing. Genuinely fears a coercion pathway but in practice benefits from the prohibition's blanket ban rather than pursuing narrower safeguard-based remedies; not itself the population bearing terminal suffering.
narrative_ontology:constraint_stakeholder(dignified_death__sanctity_primary, disability_rights_absolutist_organizations, beneficiary,
    organized, generational, analytical, national).

% Campaigns and litigates to preserve or restore prohibition, treats every legalization effort as an erosion to be fought. Draws funding, membership, and political capital from maintaining the constraint; bears none of the suffering the prohibition prolongs.
narrative_ontology:constraint_stakeholder(dignified_death__sanctity_primary, prolife_advocacy_networks, agenda_setter,
    organized, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(dignified_death__sanctity_primary, prolife_advocacy_networks, beneficiary).

% A non-agent abstraction: the social fabric the prohibition is said to protect by treating all human life as inviolable and non-negotiable. Named for completeness; collects no rents and takes no action, but is invoked as the justificatory beneficiary of the constraint.
narrative_ontology:constraint_stakeholder(dignified_death__sanctity_primary, moral_order_of_the_community, beneficiary,
    institutional, civilizational, analytical, national).
narrative_ontology:stakeholder_non_agent(dignified_death__sanctity_primary, moral_order_of_the_community).

% Face prolonged, medically unrelievable suffering with no legal option to end life on their own terms even with full mental capacity and informed consent. Exit requires clandestine methods, traveling to jurisdictions that permit assistance (if wealthy enough), or enduring the illness to its natural conclusion. The prohibition applies regardless of documented, competent consent.
narrative_ontology:constraint_stakeholder(dignified_death__sanctity_primary, terminally_ill_patients_seeking_death, payer,
    powerless, immediate, trapped, national).

% Bear the downstream effect of a prohibition that both denies them a legal death option and, paradoxically, does nothing to prevent covert pressure toward prolonged suffering as a 'test of dignity.' Named by the reading's own logic as a population the prohibition is meant to protect, yet the reading's critics note the same population is coercively prolonged in suffering by families or institutions unable to offer legal alternatives.
narrative_ontology:constraint_stakeholder(dignified_death__sanctity_primary, elderly_dependents_under_family_pressure, payer,
    powerless, immediate, trapped, national).

% Cannot afford high-quality palliative or hospice care that might make natural death more bearable, and are simultaneously denied any legal assisted-dying option. The prohibition falls hardest on those without private means to buy either good palliative care or travel-based legal exit.
narrative_ontology:constraint_stakeholder(dignified_death__sanctity_primary, low_income_patients_without_palliative_access, payer,
    powerless, immediate, trapped, national).

% Some disabled individuals with full decisional capacity and unrelieved suffering want the same end-of-life choice available to others; the sanctity-primary reading denies this categorically, treating their consent as irrelevant to transcendent moral law regardless of individual circumstance.
narrative_ontology:constraint_stakeholder(dignified_death__sanctity_primary, disabled_people_denied_agency_over_own_death, payer,
    powerless, biographical, trapped, national).

% Live with severe, treatment-resistant chronic conditions that do not meet narrow terminal-illness definitions in jurisdictions with any legal exception, and are wholly barred under sanctity-primary regimes. No legal pathway exists no matter how documented or persistent the suffering.
narrative_ontology:constraint_stakeholder(dignified_death__sanctity_primary, chronic_pain_sufferers_without_legal_recourse, payer,
    powerless, biographical, trapped, national).

% Write and adjudicate the statutes criminalizing or restricting assisted dying, balancing religious and moral lobbying against patient-rights litigation and public opinion. Their exit is constrained by electoral and judicial-legitimacy pressures; they administer the prohibition but do not personally bear its costs.
narrative_ontology:constraint_stakeholder(dignified_death__sanctity_primary, legislators_and_courts, agenda_setter,
    institutional, generational, constrained, national).

% Provide end-of-life care within the prohibition's boundaries, sometimes ethically conflicted between the sanctity framework they may personally hold and patients' expressed wish to die. Bear moral distress as a secondary cost; can decline personal participation in a hypothetical legal regime but cannot alter the prohibition itself.
narrative_ontology:constraint_stakeholder(dignified_death__sanctity_primary, palliative_and_hospice_clinicians, observer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(dignified_death__sanctity_primary, palliative_and_hospice_clinicians, payer).

% Argue for autonomy-based or relational-autonomy legal frameworks and are structurally positioned as the opposition in legislative and judicial fights; their preferred remedies (consent-based, safeguard-based legalization) are foreclosed by the sanctity-primary reading's premise that consent is categorically irrelevant.
narrative_ontology:constraint_stakeholder(dignified_death__sanctity_primary, right_to_die_advocacy_groups, excluded,
    organized, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dignified_death__sanctity_primary, diffuse).
narrative_ontology:fixing_cost_class(dignified_death__sanctity_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The prohibition purports to coordinate collective protection of vulnerable populations against a slippery slope toward normalized killing, and to preserve a shared moral commitment that no life is disposable regardless of suffering or consent.
% TRANSFER_FUNCTION: Moves the burden of unrelievable suffering from a hypothetical future population of coerced/pressured vulnerable people (protected in the abstract) onto the actual present population of competent, consenting, suffering patients who are denied any legal exit — a transfer from a speculative harm to a certain one.
% ABSENT_VOICES: Terminally ill and disabled patients who want a legal assisted-dying option are present in litigation and public testimony but structurally excluded from the moral-law framework's deliberative premise, since the reading holds their consent categorically irrelevant to the answer — their testimony can inform *how* the law is enforced but cannot, under this reading's own axioms, change *whether* it applies.
% DISAPPEARANCE_RATIONALE: If the sanctity-primary prohibition vanished overnight, jurisdictions would default to whatever legal framework existed prior (either an autonomy-based or relational-autonomy regime, or an even older blanket ban) — clinicians, legislators, religious institutions, and patients would all immediately need to renegotiate what a legal death pathway looks like; the moral, legal, and clinical order genuinely depends on this constraint being in place to have its current shape.
% FOUNDING_PROBLEM: Historically, this reading was built to prevent unilateral killing being smuggled in under the language of mercy or consent — protecting against Nazi-era eugenic 'euthanasia' programs, coerced deaths of the poor and disabled, and abuse of medical authority over life-and-death decisions.
% FOUNDING_PROBLEM_CORROBORATION: Religious and pro-life advocacy organizations attest the founding problem (protection of the vulnerable from coerced death) remains fully live. Independent bioethicists, disability-rights scholars who support narrow legalization with safeguards, and comparative-law researchers studying jurisdictions with regulated assisted dying (e.g. Oregon, Netherlands, Belgium) attest that empirical abuse rates under safeguarded legal regimes have not borne out the feared slippery slope at the scale predicted, and argue the founding problem has been substantially, though not universally, addressed by procedural safeguards rather than blanket prohibition — this corroboration comes from outside the beneficiary set (academic and comparative-policy sources, not the advocacy organizations themselves).
narrative_ontology:disappearance_verdict(dignified_death__sanctity_primary, world_rearranges).
narrative_ontology:founding_problem_status(dignified_death__sanctity_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dignified_death__sanctity_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(dignified_death__sanctity_primary, 'none', 1).
narrative_ontology:epsilon_provenance(dignified_death__sanctity_primary, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dignified_death__sanctity_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(dignified_death__sanctity_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(dignified_death__sanctity_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58 at interval end) reflects the widening gap between the prohibition's stated protective function and its lived effect on the powerless payer seats, tracked as legalization pressure and comparative-jurisdiction evidence has accumulated over the interval. Suppression (0.71) is high and rising because maintaining a blanket, consent-irrelevant prohibition in the face of visible individual suffering requires increasingly active legal and rhetorical enforcement — criminal statutes, professional sanctions against clinicians, and sustained advocacy funding. Theater ratio (0.42) reflects a growing share of the prohibition's defense resting on slippery-slope rhetoric and abstract 'moral order' invocation rather than case-specific evidence of coercion, as safeguarded legal regimes elsewhere fail to produce the predicted abuse at scale. Accessibility collapse (0.48) is moderate, not near-mountain levels, because alternative frameworks (autonomy-primary, relational-autonomy) remain visibly live in comparative law and public debate — this is not a constraint that has fully foreclosed its alternatives, which is itself evidence against a mountain reading. Resistance (0.62) is substantial: patients, disability-rights dissenters (a faction distinct from the absolutist organizations), and right-to-die advocates actively contest the prohibition in courts and legislatures.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter/beneficiary seats (religious institutions, pro-life networks, legislators enacting the prohibition), the arrangement reads as principled moral coordination — protecting an intrinsic good that cannot be waived by individual consent. From the payer seats (terminally ill, disabled, elderly, low-income patients), the identical structure operates as coercive prolongation: their competent, documented wishes are categorically overridden, and the suffering the prohibition imposes is certain and present while the harms it purports to prevent (coercion of others) are speculative and, per comparative evidence, only partially realized under safeguarded alternatives. The engine's per-seat computation should reflect this divergence sharply given the trapped exit options and powerless power atom of the payer seats versus the institutional/organized power atoms and analytical exit options of the beneficiary seats.
 *
 * DIRECTIONALITY LOGIC:
 *   Religious institutions and prolife networks are declared beneficiaries with institutional/organized power and analytical exit — they set and defend the constraint without personally bearing the suffering it prolongs, pushing their directionality toward the beneficiary end. Terminally ill, elderly, low-income, disabled, and chronic-pain payer populations are trapped (no legal exit within jurisdiction, and cross-border exit gated by wealth), which pushes their directionality toward the full-target end regardless of their formal 'protected' status under the reading's own stated rationale. The elderly_dependents_under_family_pressure stakeholder is deliberately double-edged: the reading names them as the protected class, but their trapped exit options and powerless power atom place them structurally with the other payer seats rather than with the beneficiaries — this is the structural irony the snare classification exists to surface.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing coerced or eugenic killing under cover of mercy) was genuinely live historically and remains partially live wherever safeguards are weak or absent. But the R5 corroboration shows independent, non-beneficiary sources (comparative-law researchers, some disability scholars) attesting that safeguarded legal regimes have not reproduced the predicted abuse at the feared scale, while the certain cost (prolonged unrelieved suffering among competent, consenting patients) is undisputed. The founding_problem_status of 'contested' combined with the disappearance_verdict of 'world_rearranges' is exactly the mismatch this framework is built to flag: a genealogy that is not simply dead (some coercion risk is real) nor simply live (the blanket, consent-irrelevant form of the prohibition is not the only or best-evidenced way to address that risk) — the classification as snare captures that the protective coordination function, to the extent real, could be served by narrower safeguard-based frameworks (the relational_autonomy reading) without the blanket suppression of consent this reading imposes.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_selection_ambiguity,
    'Is the sanctity_primary reading the correct grounding for legal policy on assisted dying, or do autonomy_primary / relational_autonomy readings better capture the actual moral and empirical structure of end-of-life decision-making?',
    'This is not resolvable by data alone — it is a foundational disagreement about where dignity is located (intrinsic life-value vs. self-determination vs. relational context). Comparative jurisdictional outcomes (abuse rates, palliative access, patient-reported suffering) can inform but not settle the underlying premise. The disagreement is philosophical/theological at its root, though its downstream empirical claims (slippery-slope rates) are testable.',
    'If autonomy_primary or relational_autonomy readings are judged structurally superior, the sanctity_primary prohibition''s protective function is largely subsumed by narrower safeguard mechanisms, and its remaining extraction (prolonged suffering of competent consenting patients) becomes harder to justify as necessary coordination cost.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_ambiguity, preference, 'Which kernel reading (sanctity, autonomy, or relational-autonomy) should ground legal policy is a values-level disagreement, not an empirical one.').

omega_variable(
    coercion_risk_magnitude_under_safeguards,
    'Under a safeguarded legal assisted-dying regime (informed consent, waiting periods, capacity assessment, independent review), what is the actual magnitude of coercive pressure on elderly, disabled, and low-income populations, versus the sanctity-primary reading''s predicted slippery slope?',
    'Longitudinal comparative study of jurisdictions with regulated assisted dying (Oregon, Netherlands, Belgium, Canada) tracking demographic composition of assisted deaths, documented coercion cases, and trend lines over decades of operation, cross-referenced against jurisdictions maintaining sanctity-primary prohibitions for baseline suffering and covert-death rates.',
    'If coercion risk under safeguards is low and roughly stable, the sanctity-primary reading''s central empirical premise (that any consent-based legalization inevitably produces coercive prolongation-in-reverse of the vulnerable) is substantially weakened, supporting reclassification toward relational_autonomy as the better-fit protective framework. If coercion risk under safeguards is high and rising, the sanctity-primary reading''s protective claim gains empirical support.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coercion_risk_magnitude_under_safeguards, empirical, 'Whether safeguarded legalization produces the coercive slippery slope the sanctity-primary reading predicts.').

omega_variable(
    moral_order_beneficiary_status,
    'Is ''moral order of the community'' a genuine collective good that is degraded by legalization, or is it a non-agent abstraction whose invocation primarily serves the institutional actors (religious bodies, advocacy networks) who claim to speak for it?',
    'Examine whether communities that have legalized assisted dying under safeguards show measurable degradation in other life-protective norms (e.g., suicide rates among non-terminal populations, elder abuse rates, disability discrimination indices) versus communities that have not — a genuine moral-order harm should show cross-domain effects, not just the targeted policy change.',
    'If no measurable cross-domain degradation is found, ''moral order'' functions as a rhetorical placeholder for institutional interest rather than a real collective good, strengthening the snare classification. If measurable degradation is found, the beneficiary is more genuinely collective and the coordination function is stronger than the snare classification allows.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(moral_order_beneficiary_status, conceptual, 'Whether the invoked collective beneficiary (moral order) is a real good or a rhetorical cover for institutional interest.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dignified_death__sanctity_primary, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dign_tr_t0, dignified_death__sanctity_primary, theater_ratio, 0, 0.25).
narrative_ontology:measurement(dign_tr_t8, dignified_death__sanctity_primary, theater_ratio, 8, 0.29).
narrative_ontology:measurement(dign_tr_t16, dignified_death__sanctity_primary, theater_ratio, 16, 0.33).
narrative_ontology:measurement(dign_tr_t24, dignified_death__sanctity_primary, theater_ratio, 24, 0.37).
narrative_ontology:measurement(dign_tr_t32, dignified_death__sanctity_primary, theater_ratio, 32, 0.4).
narrative_ontology:measurement(dign_tr_t40, dignified_death__sanctity_primary, theater_ratio, 40, 0.42).

% Extraction over time
narrative_ontology:measurement(dign_be_t0, dignified_death__sanctity_primary, base_extractiveness, 0, 0.44).
narrative_ontology:measurement(dign_be_t8, dignified_death__sanctity_primary, base_extractiveness, 8, 0.48).
narrative_ontology:measurement(dign_be_t16, dignified_death__sanctity_primary, base_extractiveness, 16, 0.51).
narrative_ontology:measurement(dign_be_t24, dignified_death__sanctity_primary, base_extractiveness, 24, 0.54).
narrative_ontology:measurement(dign_be_t32, dignified_death__sanctity_primary, base_extractiveness, 32, 0.56).
narrative_ontology:measurement(dign_be_t40, dignified_death__sanctity_primary, base_extractiveness, 40, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(dign_su_t0, dignified_death__sanctity_primary, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(dign_su_t8, dignified_death__sanctity_primary, suppression_requirement, 8, 0.6).
narrative_ontology:measurement(dign_su_t16, dignified_death__sanctity_primary, suppression_requirement, 16, 0.64).
narrative_ontology:measurement(dign_su_t24, dignified_death__sanctity_primary, suppression_requirement, 24, 0.67).
narrative_ontology:measurement(dign_su_t32, dignified_death__sanctity_primary, suppression_requirement, 32, 0.69).
narrative_ontology:measurement(dign_su_t40, dignified_death__sanctity_primary, suppression_requirement, 40, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dignified_death__sanctity_primary, identity_coordination).
narrative_ontology:boltzmann_floor_override(dignified_death__sanctity_primary, 0.08).
narrative_ontology:affects_constraint(dignified_death__sanctity_primary, dignified_death__autonomy_primary).
narrative_ontology:affects_constraint(dignified_death__sanctity_primary, dignified_death__relational_autonomy).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the dignified_death kernel, each authored as a separate ε-invariant story per the ε-invariance principle: dignified_death__sanctity_primary (this story, snare — high ε 0.58, victim set = vulnerable/trapped patients, beneficiary = moral/institutional order), dignified_death__autonomy_primary (expected rope-leaning or tangled_rope, low-to-moderate ε, beneficiary = the autonomous patient, minimal victim set), and dignified_death__relational_autonomy (expected tangled_rope or scaffold, moderate ε, distributed beneficiary/payer structure across patient-family-clinician triad). Each reading's ε is assessed against its own standing-arrangement referent (the reading's endorsed legal regime as contested), not against the others. They are linked here for contamination/family analysis, not merged into a single measurement.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
