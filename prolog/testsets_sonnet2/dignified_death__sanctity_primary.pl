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
 *   constraint_id: dignified_death__sanctity_primary
 *   human_readable: Sanctity-of-Life Prohibition on Assisted Death
 *   domain: bioethics/medical_law/political_philosophy
 *
 * SUMMARY:
 *   This story instantiates the sanctity_primary reading of the
 *   dignified_death kernel: dignity is located in life's intrinsic,
 *   transcendent value, and intentional life-termination is categorically
 *   wrong regardless of the individual's own consent. Under this reading, the
 *   standing arrangement being assessed is the CURRENT categorical legal
 *   prohibition on assisted death as it operates in most jurisdictions,
 *   evaluated by the sanctity reading's own lights. As the reading itself
 *   sees it, the prohibition is intended as coordination protecting a shared,
 *   non-negotiable moral floor — but the reading's own metrics show the same
 *   categorical rule falling hardest on the people it claims to protect (the
 *   elderly, disabled, and poor), while relieving no identifiable party of
 *   comparable cost. That combination — real coordination language,
 *   concentrated cost on a structurally powerless population, active
 *   enforcement via criminal and professional liability — is why the sanctity
 *   reading's own operation classifies as snare rather than the pure Mountain
 *   or Rope its rhetoric claims. The sibling readings (autonomy_primary,
 *   relational_autonomy) are separate constraints with their own ε and
 *   beneficiary/victim structures — they are not blended into this story.
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
narrative_ontology:human_readable(dignified_death__sanctity_primary, "Sanctity-of-Life Prohibition on Assisted Death").
narrative_ontology:topic_domain(dignified_death__sanctity_primary, "bioethics/medical_law/political_philosophy").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dignified_death__sanctity_primary, 'b99a5f70-55b5-419c-a883-df758aed920d').
narrative_ontology:cs_kernel_codification('b99a5f70-55b5-419c-a883-df758aed920d', distributed).
narrative_ontology:cs_authority_grounding('b99a5f70-55b5-419c-a883-df758aed920d', lineage).
narrative_ontology:cs_interpretation_layer_present('b99a5f70-55b5-419c-a883-df758aed920d').
narrative_ontology:cs_reading_relation('b99a5f70-55b5-419c-a883-df758aed920d', dignified_death__autonomy_primary, forecloses).
narrative_ontology:cs_reading_relation('b99a5f70-55b5-419c-a883-df758aed920d', dignified_death__relational_autonomy, influences).
narrative_ontology:cs_axiom('b99a5f70-55b5-419c-a883-df758aed920d', foundational, life_possesses_intrinsic_inviolable_value).
narrative_ontology:cs_axiom_status(life_possesses_intrinsic_inviolable_value, holdable).
narrative_ontology:cs_axiom_grounding('b99a5f70-55b5-419c-a883-df758aed920d', life_possesses_intrinsic_inviolable_value, deontological).
narrative_ontology:cs_axiom('b99a5f70-55b5-419c-a883-df758aed920d', foundational, consent_cannot_license_termination_of_intrinsic_value).
narrative_ontology:cs_axiom_status(consent_cannot_license_termination_of_intrinsic_value, holdable).
narrative_ontology:cs_axiom_grounding('b99a5f70-55b5-419c-a883-df758aed920d', consent_cannot_license_termination_of_intrinsic_value, theological).
narrative_ontology:cs_reference_frame('b99a5f70-55b5-419c-a883-df758aed920d', transcendent_sanctity_of_life_doctrine).
narrative_ontology:cs_drift_state('b99a5f70-55b5-419c-a883-df758aed920d', contemporary_secular_pluralist_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('b99a5f70-55b5-419c-a883-df758aed920d', '').
narrative_ontology:cs_kernel_id(dignified_death__sanctity_primary, dignified_death).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dignified_death__sanctity_primary, religious_institutions).
narrative_ontology:constraint_beneficiary(dignified_death__sanctity_primary, disability_rights_advocacy_orgs).
narrative_ontology:constraint_beneficiary(dignified_death__sanctity_primary, palliative_care_industry).
narrative_ontology:constraint_beneficiary(dignified_death__sanctity_primary, moral_order_defenders).
narrative_ontology:constraint_victim(dignified_death__sanctity_primary, terminally_ill_patients).
narrative_ontology:constraint_victim(dignified_death__sanctity_primary, elderly_dependent_populations).
narrative_ontology:constraint_victim(dignified_death__sanctity_primary, low_income_chronically_ill).
narrative_ontology:constraint_victim(dignified_death__sanctity_primary, disabled_persons_denied_agency).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(dignified_death__sanctity_primary, family_caregivers).
narrative_ontology:constraint_victim(dignified_death__sanctity_primary, family_caregivers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Legislators, religious bodies, and bioethics commissions that maintain statutory and doctrinal prohibitions on assisted dying, framing the ban as protection of an intrinsic, non-negotiable value that exists independent of any individual's stated wishes. They set the legal default that all other seats operate under and are largely insulated from the suffering the prohibition imposes on others.
narrative_ontology:constraint_stakeholder(dignified_death__sanctity_primary, moral_order_defenders, agenda_setter,
    institutional, civilizational, analytical, national).
narrative_ontology:stakeholder_secondary_role(dignified_death__sanctity_primary, moral_order_defenders, beneficiary).

% Derive doctrinal authority and social standing from being the recognized guardians of a transcendent account of dignity. The prohibition validates their moral framework as binding on the secular state and channels public deference toward their institutional role, independent of whether the terminally ill congregants they minister to would themselves choose otherwise.
narrative_ontology:constraint_stakeholder(dignified_death__sanctity_primary, religious_institutions, beneficiary,
    institutional, civilizational, arbitrage, national).

% Face prolonged, often severe suffering at end of life with no legal option to hasten death even when consistently, competently requesting it. Their consent is structurally overridden by a moral claim asserted on their behalf; exit requires either enduring the full course of illness, seeking risky unregulated means, or traveling abroad if resources permit — an option almost none of them have.
narrative_ontology:constraint_stakeholder(dignified_death__sanctity_primary, terminally_ill_patients, payer,
    powerless, immediate, trapped, local).

% Live under a prohibition justified partly as their protection from coercion, yet experience the ban itself as an extension of dependency — years of diminishing capacity and family/institutional burden with no legally sanctioned off-ramp, regardless of their own settled wishes. The protective rationale is asserted over them rather than negotiated with them.
narrative_ontology:constraint_stakeholder(dignified_death__sanctity_primary, elderly_dependent_populations, payer,
    powerless, immediate, trapped, local).

% Lack the resources for either high-quality palliative care or travel to jurisdictions permitting assisted death, so the prohibition falls on them with the least mitigation. They are simultaneously invoked as the population the ban 'protects from coercion' — the same population left with the fewest resources to escape prolonged suffering.
narrative_ontology:constraint_stakeholder(dignified_death__sanctity_primary, low_income_chronically_ill, payer,
    powerless, biographical, trapped, local).

% Disability-rights framing is invoked by moral-order defenders to justify the prohibition (protecting disabled people from pressure to die), but individual disabled adults who want the option for themselves are rarely asked directly — their voice is spoken for by advocacy organizations, not solicited as decision-makers.
narrative_ontology:constraint_stakeholder(dignified_death__sanctity_primary, disabled_persons_denied_agency, payer,
    powerless, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(dignified_death__sanctity_primary, disabled_persons_denied_agency, excluded).

% Gain political standing, funding, and moral authority by positioning themselves as the protective voice against a slippery slope toward pressured death of disabled people. Their institutional relevance is partly constituted by the existence and defense of the prohibition, even where individual disabled people's stated preferences diverge from the organization's official position.
narrative_ontology:constraint_stakeholder(dignified_death__sanctity_primary, disability_rights_advocacy_orgs, beneficiary,
    organized, generational, arbitrage, national).

% Receives sustained demand, funding, and professional legitimacy from being positioned as the sanctioned alternative to assisted death. A legal option for hastened death would not eliminate this sector but would end its status as the only legally available response to intractable end-of-life suffering, altering its bargaining position with patients and payers.
narrative_ontology:constraint_stakeholder(dignified_death__sanctity_primary, palliative_care_industry, beneficiary,
    organized, generational, mobile, national).

% Absorb years of caregiving burden, financial strain, and emotional exhaustion when a loved one's death is prolonged against their own stated wishes; some also derive meaning and continued relational time from the prohibition. Their exit options are constrained by love, obligation, and lack of legal alternatives to offer their family member.
narrative_ontology:constraint_stakeholder(dignified_death__sanctity_primary, family_caregivers, payer,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(dignified_death__sanctity_primary, family_caregivers, beneficiary).

% Bound by professional and criminal liability to withhold assistance even when they judge a patient's suffering irremediable and consent settled; their clinical judgment is subordinated to the categorical prohibition and they have no lawful channel to act on a contrary conscience-based judgment favoring the patient's request.
narrative_ontology:constraint_stakeholder(dignified_death__sanctity_primary, medical_practitioners, excluded,
    moderate, biographical, constrained, national).

% Adjudicate challenges to the prohibition, weigh competing claims about dignity, consent, and vulnerability, and can revise or uphold the statutory framework. They hear testimony from all other seats without being bound to any one party's account.
narrative_ontology:constraint_stakeholder(dignified_death__sanctity_primary, bioethics_courts_and_legislatures, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dignified_death__sanctity_primary, diffuse).
narrative_ontology:fixing_cost_class(dignified_death__sanctity_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a single, non-negotiable floor under end-of-life decisions so that no institution, family member, or economic pressure can lawfully induce or assist a person's death — coordinating a shared societal commitment against treating any life as disposable.
% TRANSFER_FUNCTION: Moves the burden of unmitigated suffering, prolonged dependency, and loss of bodily authority from the state and third parties onto the dying individual and their immediate caregivers, while moving moral and institutional legitimacy toward the bodies that enforce and interpret the prohibition.
% ABSENT_VOICES: Individual terminally ill and disabled people who would choose assisted death for themselves are represented in the public debate almost entirely through advocacy organizations and religious institutions speaking on their behalf, or through courts hearing test cases years after their deaths; their contemporaneous, competent voice is structurally absent from the legislative process that sets the rule governing them.
% DISAPPEARANCE_RATIONALE: If the prohibition vanished overnight, end-of-life medicine, palliative care financing, disability advocacy positioning, and religious institutional authority over death would all have to reorganize around a consent-based framework; family caregiving burdens would shift, and a new set of procedural safeguards (or their absence) would immediately become the live political question — the arrangement is load-bearing for multiple institutions, not a neutral background fact.
% FOUNDING_PROBLEM: Historically the prohibition was built to prevent a class of harms: coerced or hasty killing of the vulnerable, medical abandonment of the sick, and erosion of a shared taboo against treating human life as disposable in the aftermath of eugenics-era abuses.
% FOUNDING_PROBLEM_CORROBORATION: Moral-order defenders and religious institutions attest the founding problem (protection from coercion and devaluation of vulnerable life) remains fully live. Independent sources outside the beneficiary set — palliative care physicians' associations in jurisdictions that have legalized regulated assisted dying, and empirical studies from those jurisdictions showing no measurable increase in coercion of vulnerable groups under safeguarded regimes — report the original harm the ban targets is substantially addressable through procedural safeguards short of categorical prohibition, suggesting the founding problem has been partially superseded by regulatory alternatives the categorical rule does not credit.
narrative_ontology:disappearance_verdict(dignified_death__sanctity_primary, world_rearranges).
narrative_ontology:founding_problem_status(dignified_death__sanctity_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dignified_death__sanctity_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
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
 *   Extraction (ε=0.58) reflects that the prohibition transfers years of unmitigated suffering and loss of bodily authority onto a population with no lawful alternative, while institutional actors (religious bodies, advocacy organizations, palliative care sector) gain standing and resources from being the recognized stewards of the prohibited domain. Suppression (0.71) is high because exit is foreclosed by criminal and professional liability rather than by persuasion — practitioners who might otherwise assist face liability regardless of documented, competent patient consent. Theater ratio (0.42) captures that a meaningful share of the apparatus (extensive procedural review boards, ethics consultations that cannot approve the outcome being reviewed) performs deliberation without being able to reach the outcome the deliberation ostensibly considers. Accessibility collapse (0.48) is moderate rather than near-total because informal, unregulated, and cross-border alternatives persist for some — but that residual accessibility is unevenly distributed by wealth and mobility, which is itself part of the extraction pattern. Resistance (0.62) is substantial: legal challenges, right-to-die litigation, and physician civil disobedience document active, organized contest of the rule from within its own jurisdiction.
 *
 * DIRECTIONALITY LOGIC:
 *   Moral-order defenders and religious institutions sit near the beneficiary end: they set or ratify the rule, bear none of its physical cost, and gain legitimacy from its persistence. Terminally ill patients, the elderly dependent, the low-income chronically ill, and disabled adults denied individual agency sit near the full-target end: the prohibition's costs land on their bodies and their remaining biographical time, and their consent is the exact thing the constraint is structured to override. Disability rights organizations and the palliative care industry are true institutional beneficiaries even though their stated purpose is protective — their organizational standing depends in part on the prohibition's continuation, which the derivation captures as partial beneficiary status distinct from the disabled individuals whose stated wishes they do not uniformly represent.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — preventing coerced or hasty killing of the vulnerable — was real and remains partly live; the classification does not deny that a genuine coordination problem exists. What the snare classification isolates is that the categorical, non-negotiable form of the rule (rather than a procedurally safeguarded, consent-verified alternative) has calcified into an instrument that imposes cost independent of whether the specific coercion risk is present in a given case. The mismatch between founding_problem_status (contested — partially superseded by safeguard-based regulatory alternatives) and disappearance_verdict (world_rearranges) is exactly the signal this framework is built to surface: multiple institutions have real stakes in the rule's persistence beyond the residual protective function it still performs.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    transcendent_claim_vs_constructed_protection,
    'Is the categorical prohibition a direct implementation of an irreducible transcendent moral truth about life''s value, or a constructed policy instrument that uses transcendent language to justify a specific, contestable allocation of decision authority?',
    'No empirical resolution mechanism exists for the metaphysical claim itself; a partial proxy is whether procedurally safeguarded consent-based alternatives (as piloted in permissive jurisdictions) demonstrably fail to produce the coercion harms the categorical rule is meant to prevent — if safeguards perform comparably, the categorical form is doing less protective work than claimed.',
    'If the transcendent claim is doing genuine protective work not replicable by safeguards, the snare classification would need revision toward tangled_rope (real coordination function alongside real extraction) or even a contested Mountain reading; if safeguards perform comparably, the categorical form is better characterized as excess enforcement layered onto a narrower legitimate function.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(transcendent_claim_vs_constructed_protection, conceptual, 'Whether the categorical prohibition tracks an irreducible moral fact or is a constructed instrument wearing transcendent framing.').

omega_variable(
    kernel_reading_disagreement_locus,
    'Where exactly does this reading''s disagreement with autonomy_primary and relational_autonomy live — is it a disagreement about facts (does legalization increase coercion of vulnerable populations), or a disagreement about the site of moral authority (does an individual''s consent ever suffice to license their own death, regardless of facts about coercion risk)?',
    'Compare outcomes: if empirical study of safeguarded assisted-dying regimes shows no elevated coercion of vulnerable groups yet the sanctity_primary reading still opposes legalization, the disagreement is located in moral authority, not in empirical risk assessment — the transcendent claim would be doing the entire work independent of consequences.',
    'If the disagreement is purely about moral authority rather than empirical risk, then no amount of procedural safeguard evidence from the relational_autonomy sibling reading would move this reading''s classification — the two readings are not converging positions but genuinely incommensurable framings of what dignity requires.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_locus, conceptual, 'Whether sanctity_primary''s opposition to legalization is empirically or metaphysically grounded, relative to the sibling readings.').

omega_variable(
    coalition_power_of_powerless_victims,
    'Can terminally ill patients, the elderly dependent, and disabled adults denied agency — all coded powerless individually — achieve coalition power through cross-jurisdictional litigation, patient advocacy networks, or public referendum campaigns sufficient to alter the prohibition''s classification trajectory?',
    'Track referendum and litigation outcomes across jurisdictions over the measurement interval; a rising success rate for legalization campaigns driven by patient coalitions (rather than top-down legislative reform) would indicate emergent coalition power.',
    'Sustained coalition success would predict declining suppression and declining ε over time as the categorical rule erodes jurisdiction by jurisdiction, converting the story from a stable snare into a piton in transition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coalition_power_of_powerless_victims, empirical, 'Whether structurally powerless victim populations can build coalition power to contest the prohibition.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dignified_death__sanctity_primary, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dign_tr_t0, dignified_death__sanctity_primary, theater_ratio, 0, 0.3).
narrative_ontology:measurement(dign_tr_t8, dignified_death__sanctity_primary, theater_ratio, 8, 0.33).
narrative_ontology:measurement(dign_tr_t16, dignified_death__sanctity_primary, theater_ratio, 16, 0.36).
narrative_ontology:measurement(dign_tr_t24, dignified_death__sanctity_primary, theater_ratio, 24, 0.38).
narrative_ontology:measurement(dign_tr_t32, dignified_death__sanctity_primary, theater_ratio, 32, 0.4).
narrative_ontology:measurement(dign_tr_t40, dignified_death__sanctity_primary, theater_ratio, 40, 0.42).

% Extraction over time
narrative_ontology:measurement(dign_be_t0, dignified_death__sanctity_primary, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(dign_be_t8, dignified_death__sanctity_primary, base_extractiveness, 8, 0.46).
narrative_ontology:measurement(dign_be_t16, dignified_death__sanctity_primary, base_extractiveness, 16, 0.5).
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

narrative_ontology:coordination_type(dignified_death__sanctity_primary, enforcement_mechanism).
narrative_ontology:affects_constraint(dignified_death__sanctity_primary, dignified_death__autonomy_primary).
narrative_ontology:affects_constraint(dignified_death__sanctity_primary, dignified_death__relational_autonomy).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the dignified_death kernel, decomposed per the ε-invariance principle: sanctity_primary (this story, snare, ε=0.58 — categorical ban falls on vulnerable populations), autonomy_primary (individual self-determination reading, expected rope/tangled_rope with a different victim set centered on denied autonomy rather than coerced prolongation), and relational_autonomy (procedural triad reading, expected scaffold or tangled_rope with safeguard-mediated extraction). Each reading is authored as its own constraint with its own ε and beneficiary/victim structure; they are linked here rather than blended into a single averaged story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
