% ============================================================================
% CONSTRAINT STORY: dignified_death__sanctity_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
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
 *   constraint_id: dignified_death__sanctity_primary
 *   human_readable: Sanctity-of-Life Prohibition on Intentional Life-Termination (Sanctity-Primary Reading)
 *   domain: bioethics/medical law/political philosophy
 *
 * SUMMARY:
 *   This story instantiates the sanctity_primary reading of the
 *   dignified_death kernel: the constraint that intentional life-termination
 *   violates transcendent moral law regardless of consent, enforced through
 *   criminal statutes, professional sanctions, and doctrinal discipline. Per
 *   the epsilon-invariance principle the kernel decomposes into three
 *   structurally distinct constraints - this prohibition (sanctity_primary),
 *   a legal-access permission structure (autonomy_primary), and a
 *   triad-authority procedural regime (relational_autonomy) - linked through
 *   network.affects_constraints. The family's epsilon values differ because
 *   each reading fixes a different referent and evaluates it through its own
 *   premises: this reading authors epsilon 0.62 for the prohibition
 *   arrangement it defends, registering the burden the prohibition imposes
 *   regardless of consent (the reading's own defining clause) as a real cost
 *   priced as morally mandated; the autonomy reading authors its epsilon
 *   against the access denial it contests; the relational reading against
 *   procedural exclusion. Claim and metrics are independent authored facts:
 *   the claimed type (snare) states this reading's structural verdict - a
 *   protection norm whose protective story no longer covers its coercive
 *   operation - while the metrics describe the prohibition's actual operation
 *   (accumulating burden, rising enforcement intensity, growing reliance on
 *   vulnerability-protection rhetoric, wealth-stratified escape). The engine
 *   computes per-seat classifications from the structural data; where a seat
 *   computes differently from the claim, that divergence is the datum. KEY
 *   AGENTS (by structural relationship): - religious_institutions: agenda
 *   setter and principal beneficiary (institutional/identity_locked) -
 *   teaches and politically enforces the inviolability doctrine; collects
 *   doctrinal authority and communal cohesion - legislators_and_regulators:
 *   agenda setter (institutional/mobile) - enacts and sustains the
 *   prohibiting statutes; exit is ordinary legislation -
 *   terminally_ill_patients_denied_aid_in_dying: primary target
 *   (powerless/trapped) - bears refused relief and prolonged dying -
 *   disabled_patients_facing_prolonged_suffering: target
 *   (organized/constrained) - guaranteed endurance of previously refused
 *   suffering - low_income_patients_without_exit_options: target
 *   (powerless/trapped) - bears the full refusal with no purchasable exit -
 *   affluent_mobile_patients: partial-exit target (powerful/arbitrage) - buys
 *   foreign exit at the price of earlier, displaced death -
 *   family_caregivers_under_forced_burden: secondary target
 *   (moderate/constrained) - absorbs compressed-away caregiving years and
 *   prosecution exposure - palliative_care_establishment: beneficiary
 *   (organized/mobile) - receives care channeling, funding primacy,
 *   gatekeeping seats - anti_euthanasia_advocacy_organizations: beneficiary
 *   (organized/mobile) - mission, funding, and advisory standing sustained by
 *   persistence - treating_clinicians: dual-positioned (organized/mobile) -
 *   bears moral injury and legal risk while their profession's authority is
 *   policed on their behalf - autonomy_movement_advocates: contesting target
 *   (organized/constrained) - carries legal-defense costs and defeated
 *   legislative projects - bioethics_analysts: analytical observer
 *   (analytical/analytical) - maps the argument space, holds no vote
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dignified_death__sanctity_primary, 0.62).
domain_priors:suppression_score(dignified_death__sanctity_primary, 0.84).
domain_priors:theater_ratio(dignified_death__sanctity_primary, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dignified_death__sanctity_primary, extractiveness, 0.62).
narrative_ontology:constraint_metric(dignified_death__sanctity_primary, suppression_requirement, 0.84).
narrative_ontology:constraint_metric(dignified_death__sanctity_primary, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dignified_death__sanctity_primary, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(dignified_death__sanctity_primary, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dignified_death__sanctity_primary, snare).
narrative_ontology:human_readable(dignified_death__sanctity_primary, "Sanctity-of-Life Prohibition on Intentional Life-Termination (Sanctity-Primary Reading)").
narrative_ontology:topic_domain(dignified_death__sanctity_primary, "bioethics/medical law/political philosophy").

domain_priors:requires_active_enforcement(dignified_death__sanctity_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dignified_death__sanctity_primary, 'f89d056e-72d1-473b-8b02-ea2a47782a48').
narrative_ontology:cs_kernel_codification('f89d056e-72d1-473b-8b02-ea2a47782a48', fixed_text).
narrative_ontology:cs_authority_grounding('f89d056e-72d1-473b-8b02-ea2a47782a48', lineage).
narrative_ontology:cs_interpretation_layer_present('f89d056e-72d1-473b-8b02-ea2a47782a48').
narrative_ontology:cs_reading_relation('f89d056e-72d1-473b-8b02-ea2a47782a48', dignified_death__autonomy_primary, forecloses).
narrative_ontology:cs_reading_relation('f89d056e-72d1-473b-8b02-ea2a47782a48', dignified_death__relational_autonomy, coexists_with).
narrative_ontology:cs_axiom('f89d056e-72d1-473b-8b02-ea2a47782a48', foundational, life_intrinsically_inviolable).
narrative_ontology:cs_axiom_status(life_intrinsically_inviolable, holdable).
narrative_ontology:cs_axiom_grounding('f89d056e-72d1-473b-8b02-ea2a47782a48', life_intrinsically_inviolable, deontological).
narrative_ontology:cs_axiom('f89d056e-72d1-473b-8b02-ea2a47782a48', foundational, consent_does_not_legitimate_intentional_death).
narrative_ontology:cs_axiom_status(consent_does_not_legitimate_intentional_death, holdable).
narrative_ontology:cs_axiom_grounding('f89d056e-72d1-473b-8b02-ea2a47782a48', consent_does_not_legitimate_intentional_death, deontological).
narrative_ontology:cs_axiom('f89d056e-72d1-473b-8b02-ea2a47782a48', secondary, healing_killing_boundary_absolute).
narrative_ontology:cs_axiom_status(healing_killing_boundary_absolute, holdable).
narrative_ontology:cs_axiom_grounding('f89d056e-72d1-473b-8b02-ea2a47782a48', healing_killing_boundary_absolute, conventional).
narrative_ontology:cs_reference_frame('f89d056e-72d1-473b-8b02-ea2a47782a48', absolute_life_sanctity_framework).
narrative_ontology:cs_drift_state('f89d056e-72d1-473b-8b02-ea2a47782a48', contemporary_legalization_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('f89d056e-72d1-473b-8b02-ea2a47782a48', '').
narrative_ontology:cs_kernel_id(dignified_death__sanctity_primary, dignified_death).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dignified_death__sanctity_primary, religious_institutions).
narrative_ontology:constraint_beneficiary(dignified_death__sanctity_primary, palliative_care_establishment).
narrative_ontology:constraint_beneficiary(dignified_death__sanctity_primary, anti_euthanasia_advocacy_organizations).
narrative_ontology:constraint_victim(dignified_death__sanctity_primary, terminally_ill_patients_denied_aid_in_dying).
narrative_ontology:constraint_victim(dignified_death__sanctity_primary, disabled_patients_facing_prolonged_suffering).
narrative_ontology:constraint_victim(dignified_death__sanctity_primary, low_income_patients_without_exit_options).
narrative_ontology:constraint_victim(dignified_death__sanctity_primary, affluent_mobile_patients).
narrative_ontology:constraint_victim(dignified_death__sanctity_primary, family_caregivers_under_forced_burden).
narrative_ontology:constraint_victim(dignified_death__sanctity_primary, treating_clinicians).
narrative_ontology:constraint_victim(dignified_death__sanctity_primary, autonomy_movement_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(dignified_death__sanctity_primary, treating_clinicians).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain and teach the doctrine that human life is inviolable and that intentionally ending a life is never permissible, whatever the person asks for. Fund and staff campaigns against assisted-dying legislation, supply testimony in hearings, and discipline affiliated professionals and politicians who defect. Their authority and internal cohesion depend on the doctrine remaining binding; exit would mean a doctrinal reversal of the kind that has fractured other teachings.
narrative_ontology:constraint_stakeholder(dignified_death__sanctity_primary, religious_institutions, agenda_setter,
    institutional, civilizational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(dignified_death__sanctity_primary, religious_institutions, beneficiary).

% Enact and maintain the criminal statutes and professional regulations that prohibit intentional life-termination, and appoint the boards that sanction clinicians who assist. They weigh consistent majoritarian public support for legal access against organized religious opposition and electoral risk. Their way out is ordinary: pass a repeal or a regulated-access statute, as peers in other jurisdictions have done.
narrative_ontology:constraint_stakeholder(dignified_death__sanctity_primary, legislators_and_regulators, agenda_setter,
    institutional, generational, mobile, national).

% Delivers end-of-life symptom relief and argues, in funding negotiations and public debate, that good palliation removes any need for legally provided death. The prohibition channels terminal care and research funding through palliative services and gives the specialty a standing gatekeeping seat in every review commission. Its skills are portable and demand grows under any legal regime.
narrative_ontology:constraint_stakeholder(dignified_death__sanctity_primary, palliative_care_establishment, beneficiary,
    organized, generational, mobile, national).

% Run permanent campaigns against legalization on vulnerability-protection and disability-solidarity grounds. The continuing prohibition keeps their missions funded, their experts on advisory panels, and their framing central to the debate; several organizations grew directly out of the last four decades of legislative fights and exist to fight the next one.
narrative_ontology:constraint_stakeholder(dignified_death__sanctity_primary, anti_euthanasia_advocacy_organizations, beneficiary,
    organized, generational, mobile, national).

% Face months of escalating symptoms with legal access to relief deliberately withheld; requests are refused, counselors risk sanctions, and the only lawful path is palliative sedation on clinical terms. Some travel abroad while still well enough to travel, dying earlier than they would choose; others remain, and a minority die by violent improvised means that injure the families who find them. Exit is effectively closed: travel is expensive, physically demanding, and criminalized to assist in several home jurisdictions.
narrative_ontology:constraint_stakeholder(dignified_death__sanctity_primary, terminally_ill_patients_denied_aid_in_dying, payer,
    powerless, immediate, trapped, national).

% Live with progressive conditions that will eventually produce dependence and suffering they have stated, in advance, they do not want to endure; the prohibition guarantees they will endure it anyway. Organized advocacy exists and is heard, but it is split - much of it defends the prohibition as protection against devaluing pressure, while many individuals want the option reserved for their own future selves. Relocating to a permissive jurisdiction is rarely feasible with progressive disability.
narrative_ontology:constraint_stakeholder(dignified_death__sanctity_primary, disabled_patients_facing_prolonged_suffering, payer,
    organized, biographical, constrained, national).

% Receive the same refusals as everyone else and none of the exits: no savings for foreign clinics, no second homes, no legal fund. When suffering peaks they take what the ward offers or improvise alone. They are the population the paid cross-border route skips entirely.
narrative_ontology:constraint_stakeholder(dignified_death__sanctity_primary, low_income_patients_without_exit_options, payer,
    powerless, immediate, trapped, national).

% Can buy the exit others are denied - a clinic abroad, a lawyer, a planned date - but purchase it at the price of leaving home weeks or months before they otherwise would, dying among strangers in a foreign facility, and exposing accompanying relatives to investigation at home. Their money converts a total refusal into an expensive, early, displaced one.
narrative_ontology:constraint_stakeholder(dignified_death__sanctity_primary, affluent_mobile_patients, payer,
    powerful, biographical, arbitrage, global).

% Absorb years of physical care, income loss, and anticipatory grief that a legal assisted-death option would compress; simultaneously they fear suspicion if a relative dies suddenly at home, and some have been prosecuted for compassionate acts. Their exit is private and costly: institutional placement, or enduring.
narrative_ontology:constraint_stakeholder(dignified_death__sanctity_primary, family_caregivers_under_forced_burden, payer,
    moderate, biographical, constrained, national).

% Watch patients suffer past the point the patients themselves named as intolerable, and risk license and liberty if they ease death rather than merely lengthen it; several have been investigated or charged. At the same time their profession's authority rests on the old boundary between healing and killing, which the prohibition polices on their behalf.
narrative_ontology:constraint_stakeholder(dignified_death__sanctity_primary, treating_clinicians, payer,
    organized, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(dignified_death__sanctity_primary, treating_clinicians, beneficiary).

% Campaign for legal access, counsel the desperate, and in some cases personally assist deaths that land them in court; their legislative projects are repeatedly defeated and their organizations carry legal-defense costs as a standing expense.
narrative_ontology:constraint_stakeholder(dignified_death__sanctity_primary, autonomy_movement_advocates, payer,
    organized, generational, constrained, national).

% Map the argument space across the competing dignity readings, audit the empirical record from permissive and prohibitive jurisdictions alike, and advise commissions without holding a vote in any of them.
narrative_ontology:constraint_stakeholder(dignified_death__sanctity_primary, bioethics_analysts, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dignified_death__sanctity_primary, religious_institutions).
narrative_ontology:fixing_cost_class(dignified_death__sanctity_primary, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a real collective-action problem at the margin: a shared, enforceable commitment that no one intentionally ends a life maintains baseline trust between dependents and their caretakers (the dependent need not prove ongoing usefulness to stay safe), preserves a bright line between healing and killing on which medical credibility trades, and pools protection for people unable to defend their own interests at the end of life. Stated without evaluation; the dispute is over whether blanket refusal is the necessary form of that commitment.
% TRANSFER_FUNCTION: Moves final authority over the timing and manner of death from suffering individuals to the moral-legal order (state statutes, professional boards, doctrinal authorities); moves the costs of prolonged dying onto patients and their unpaid family caregivers; and channels end-of-life spending and professional attention through palliative institutions.
% ABSENT_VOICES: The dying themselves hold no formal seat: prohibition regimes legislate and review end-of-life law through bodies composed of people who are not currently dying, and patient requests enter only as cases to be refused. Bereaved families of patients who died by violent improvised means after refusal testify, if at all, only informally; coroner files and hospice wards, not committee rooms, are where they are found.
% DISAPPEARANCE_RATIONALE: If the prohibition vanished overnight, jurisdictions would move quickly to safeguarded legal access - public support runs at supermajority levels across surveyed democracies - medical practice would reorganize around eligibility protocols, palliative funding arguments would restructure, and religious institutions would lose a central disciplinary instrument. The end-of-life economy visibly rearranges wherever repeal has occurred.
% FOUNDING_PROBLEM: Preventing wrongful killing: protecting the dependent - the elderly, the ill, the disabled - from being killed or pressured into death by heirs, exhausted families, or utilitarian calculation, and maintaining the absolute distinction between healing and killing on which medical trust rests. Rooted in religious teaching that life is not one's own to dispose of, and in the Hippocratic separation of the physician's role from killing.
% FOUNDING_PROBLEM_CORROBORATION: Corroboration from outside the beneficiary set: comparative jurisprudence and official review bodies in permissive jurisdictions (Oregon's multi-decade report series, Dutch regional review committees, Canadian expert panels) attest that the founding problem is real but is addressed by capacity assessment, waiting periods, and independent verification rather than blanket prohibition; courts in several jurisdictions have found absolute bans disproportionate to that aim. Corroboration that blanket prohibition specifically remains necessary comes almost entirely from religious institutions and allied advocacy organizations - the parties that benefit from it - and no neutral body attests it; that absence is itself signal.
narrative_ontology:disappearance_verdict(dignified_death__sanctity_primary, world_rearranges).
narrative_ontology:founding_problem_status(dignified_death__sanctity_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dignified_death__sanctity_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(dignified_death__sanctity_primary, 'none', 1).
narrative_ontology:epsilon_provenance(dignified_death__sanctity_primary, 0.62, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness 0.62 sits in the manifest's 0.50-0.65 band and is reading-indexed: it registers the burden this prohibition imposes on governed parties without their consent - refused relief, prolonged dying, criminalized assistance - a burden the reading itself stipulates is non-consensual by construction ('regardless of consent') even while pricing it as morally mandated. Suppression 0.84 is a raw structural property, unscaled by power or scope: criminal statutes, license revocation, prosecution of assisting relatives and physicians, and border enforcement carry it, roughly three-quarters structural and one-quarter internalized (patients formed under the norm internalize a duty to endure; clinicians internalize the killing taboo). Theater ratio 0.31: enforcement remains substantively functional, but the performative share grows as theological motivation loses cultural traction and is replaced by ritually deployed secular vulnerability-protection arguments in hearings that rarely change outcomes. Accessibility collapse 0.62: alternatives collapse almost completely for the poor and partially for the affluent, whose paid cross-border exit survives - the collapse is wealth-stratified. Resistance 0.70: sustained legislative campaigns, litigation, and persistent supermajority opinion favoring access. The measurement series run on ONE shared grid (T=0..55, calibrated 1970-2025: Quinlan era, Kevorkian prosecutions, Oregon operationalization, Benelux statutes, Canadian MAiD, expansion waves), with all three metrics authored at all seven points. Base extractiveness accumulates as life-extension technology widens the gap between foreclosed relief and endured suffering; suppression_requirement rises 0.55 to 0.84 modeling an enforcement ratchet - compliance once carried by social consensus required explicit machinery as dissent grew. Coalition note: the victim class is structurally self-limiting - it turns over completely as cohorts die, and each member's horizon is measured in months, foreclosing the durable organization that coalition power for powerless agents normally requires; the constraint's severest costs fall on those least positioned to contest them. Receipt surface: gains demonstrably accrue to the religious seat (authority, cohesion, disciplinary leverage), so gain_flow names it rather than asserting diffusion. Fixing cost is 'cheap': repeal is ordinary legislation, demonstrated in more than a dozen jurisdictions, and costs less than the benefit - the prohibition persists not because fixing is expensive but because the capturer is concentrated and motivated while the victims are dying and disorganized.
 *
 * PERSPECTIVAL GAP:
 *   Seat divergence is extreme. From the religious seat the arrangement is a sacred obligation - the engine should compute a coordination-flavored experience there (low directionality, identity-locked enforcement, generational-plus horizon). From the trapped patient seats the same statutes operate as pure refusal - maximal directionality, immediate horizons, no exit. Legislators occupy an electoral-tradeoff seat with mobile exit; the palliative seat experiences professional vindication and funding primacy. Same-level lateral differentiation is sharpest here: two patients with identical diagnoses and prognoses differ in outcome ONLY by wealth - one exits through paid arbitrage, one is trapped - so power diverges despite equal biological standing because the constraint's exit structure is priced in currency. Identity-lock dynamics: the religious seat's exit is authored identity_locked on doctrinal-fusion grounds - the institution has become its teaching; if that frame broke (as prior doctrinal reversals show is possible), enforcement would soften quickly, because the secular cover arguments are weaker than the doctrinal core they front for.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries (religious_institutions, palliative_care_establishment, anti_euthanasia_advocacy_organizations) derive low directionality - the constraint subsidizes them. Declared victims derive high directionality scaled by exit: trapped seats (terminally_ill_patients_denied_aid_in_dying, low_income_patients_without_exit_options) sit nearest the full-target end; constrained seats (disabled patients, caregivers, clinicians, autonomy advocates) next; the arbitrage seat (affluent_mobile_patients) would be damped toward the beneficiary end by the automatic derivation, which is WRONG here - their exit is purchased at the price of earlier-than-chosen foreign death, solitary dying, and family legal exposure, so a directionality override holds them at 0.52, above symmetric. Agenda setters without declarations (legislators_and_regulators) fall to the canonical fallback. Scope amplification applies engine-side: the prohibition operates at national-to-global scope where verification of coercion and suffering is hardest.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards both error directions. Authoring beneficiaries, active enforcement, and named victims prevents the protection story from laundering the arrangement as pure coordination or as moral law: the snare verdict records that the protective function, whatever its sincerity, no longer accounts for the arrangement's operation, which is refusal maintained by coercion with a concentrated capturer. Conversely it resists the mirror error of reading the whole arrangement as bad-faith performance: theater_ratio 0.31 records that most enforcement is sincere. Genealogy: the founding problem (preventing wrongful killing of the dependent) retains a live core that safeguarded systems address by other means, so founding_problem_status is contested rather than dead; with disappearance_verdict world_rearranges, the mismatch consumer finds no zombie flag - the arrangement persists because the world is arranged around it and its beneficiaries defend it, not because its mandate has silently expired.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_structure,
    'This constraint is one reading of the dignified_death kernel (reading: sanctity_primary). What structurally changes if a sibling reading is adopted instead?',
    'Comparative classification across the three reading-stories of the kernel: diff the victim sets, beneficiary sets, and epsilon values. The disagreement is located in the locus-of-dignity premise (intrinsic life value vs self-determination vs relational context), which determines who counts as protected and who as burdened.',
    'Adopting autonomy_primary empties this reading''s victim set of coerced prolongation and creates a new one (patients denied access); adopting relational_autonomy redistributes agenda authority to the patient-family-clinician triad and replaces blanket refusal with procedural gating. Classification of THIS story is unaffected - the reading is held fixed here.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Committer structure: one reading of the dignified_death kernel; sibling readings relocate the victim set and the dignity premise.').

omega_variable(
    transcendent_grounding_status,
    'Is the prohibition''s grounding genuinely transcendent moral law - mind-independent and binding regardless of enactment - or a constructed doctrinal-political arrangement maintained by identifiable institutions with material interests in its persistence?',
    'Cross-cultural metaethical survey plus institutional history: test whether the norm''s operative content tracks the enforcing institutions'' interests across contexts where those interests diverge, including traditions that reversed adjacent absolutes while retaining this one.',
    'If constructed, the arrangement forfeits any natural-law immunity pretension and classifies plainly as enforced extraction with concentrated beneficiaries; if transcendent, its persistence is not evidence of capture and the beneficiary profile is incidental.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transcendent_grounding_status, conceptual, 'Natural-law vs constructed grounding of the inviolability norm.').

omega_variable(
    coercion_direction_ambiguity,
    'Does the prohibition protect vulnerable populations from pressure toward death, or coerce them into prolonged suffering - and in what measurable proportions do both effects coexist?',
    'Jurisdictional natural experiments: post-legalization coercion epidemiology (Oregon, Benelux, Canadian review-committee findings) against violent-self-deliverance rates, unrelieved-symptom prevalence, and caregiver-burden data in prohibition regimes.',
    'Protection-dominant results push the structure toward tangled_rope (genuine coordination plus asymmetric extraction); prolongation-dominant results confirm the snare reading and raise effective extraction for trapped seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coercion_direction_ambiguity, empirical, 'Direction of coercive force: shield for the vulnerable or forced prolongation.').

omega_variable(
    epsilon_valuation_index,
    'Epsilon is authored at 0.62 as this reading''s registration of the burden imposed regardless of consent, yet the reading prices that burden as morally mandated rather than unjust - does the deontological valuation discount the burden the engine should register?',
    'Engine-side comparison of computed chi across the three sibling stories over their respective referents, holding structural data constant; inspect whether the reading-indexed valuation systematically dampens extraction relative to the autonomy reading''s indexing.',
    'If the valuation discounts, computed extraction falls toward tangled_rope territory despite identical structural declarations; if not, the snare verdict stands on the structural data alone.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(epsilon_valuation_index, conceptual, 'Reading-indexed epsilon versus deontological valuation of the same burden.').

omega_variable(
    safeguard_adequacy_question,
    'Are procedural safeguards - capacity assessment, waiting periods, independent second verification - sufficient to prevent the coercion of vulnerable populations that motivates the prohibition, rendering blanket refusal redundant protection?',
    'Longitudinal abuse epidemiology in long-running safeguarded systems, audited against prohibition-regime baselines for the same populations.',
    'Sufficient safeguards strip the protective function down to theatrical remainder and strengthen the snare verdict; demonstrated insufficiency restores a genuine protective function and supports tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(safeguard_adequacy_question, empirical, 'Whether safeguards substitute for blanket prohibition in protecting the vulnerable.').

omega_variable(
    doctrinal_identity_fusion,
    'Religious institutions'' exit is authored as identity_locked: is the fusion doctrinal-essential (abandoning inviolability would dissolve the institution''s authority to speak on ultimate questions) or strategic-retreat-avoidant (reversal possible with face-saving reinterpretation, as with prior overturned teachings)?',
    'Historical analysis of prior doctrinal reversals and their institutional costs, plus elite interviews on the perceived cost of reversing this specific teaching.',
    'If strategic, the institutions'' directionality rises (they could exit and decline to, from interest), sharpening the capture reading; if essential, enforcement is belief-driven and the identity lock is genuine.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(doctrinal_identity_fusion, empirical, 'Essential vs strategic character of the doctrinal identity lock.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dignified_death__sanctity_primary, 0, 55).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dign_tr_t0, dignified_death__sanctity_primary, theater_ratio, 0, 0.15).
narrative_ontology:measurement(dign_tr_t10, dignified_death__sanctity_primary, theater_ratio, 10, 0.17).
narrative_ontology:measurement(dign_tr_t20, dignified_death__sanctity_primary, theater_ratio, 20, 0.2).
narrative_ontology:measurement(dign_tr_t30, dignified_death__sanctity_primary, theater_ratio, 30, 0.23).
narrative_ontology:measurement(dign_tr_t40, dignified_death__sanctity_primary, theater_ratio, 40, 0.26).
narrative_ontology:measurement(dign_tr_t48, dignified_death__sanctity_primary, theater_ratio, 48, 0.29).
narrative_ontology:measurement(dign_tr_t55, dignified_death__sanctity_primary, theater_ratio, 55, 0.31).

% Extraction over time
narrative_ontology:measurement(dign_be_t0, dignified_death__sanctity_primary, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(dign_be_t10, dignified_death__sanctity_primary, base_extractiveness, 10, 0.44).
narrative_ontology:measurement(dign_be_t20, dignified_death__sanctity_primary, base_extractiveness, 20, 0.49).
narrative_ontology:measurement(dign_be_t30, dignified_death__sanctity_primary, base_extractiveness, 30, 0.53).
narrative_ontology:measurement(dign_be_t40, dignified_death__sanctity_primary, base_extractiveness, 40, 0.57).
narrative_ontology:measurement(dign_be_t48, dignified_death__sanctity_primary, base_extractiveness, 48, 0.6).
narrative_ontology:measurement(dign_be_t55, dignified_death__sanctity_primary, base_extractiveness, 55, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(dign_su_t0, dignified_death__sanctity_primary, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(dign_su_t10, dignified_death__sanctity_primary, suppression_requirement, 10, 0.6).
narrative_ontology:measurement(dign_su_t20, dignified_death__sanctity_primary, suppression_requirement, 20, 0.66).
narrative_ontology:measurement(dign_su_t30, dignified_death__sanctity_primary, suppression_requirement, 30, 0.72).
narrative_ontology:measurement(dign_su_t40, dignified_death__sanctity_primary, suppression_requirement, 40, 0.77).
narrative_ontology:measurement(dign_su_t48, dignified_death__sanctity_primary, suppression_requirement, 48, 0.81).
narrative_ontology:measurement(dign_su_t55, dignified_death__sanctity_primary, suppression_requirement, 55, 0.84).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dignified_death__sanctity_primary, identity_coordination).
narrative_ontology:affects_constraint(dignified_death__sanctity_primary, dignified_death__autonomy_primary).
narrative_ontology:affects_constraint(dignified_death__sanctity_primary, dignified_death__relational_autonomy).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'dignified death' decomposes into three structurally distinct constraints per the epsilon-invariance principle. This story (sanctity_primary) is the historical upstream member - doctrinally oldest, institutionally entrenched - and influences the siblings' operating environment: legalization debates occur inside legal frameworks this reading built. The autonomy_primary sibling (downstream, contested, expanding) and the relational_autonomy sibling (procedural middle position) each carry their own epsilon, victim set, and claimed type; the upstream reading's prohibition is frequently cited as evidence within the downstream debates, which is why the edges run from this story to both siblings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(dignified_death__sanctity_primary, powerful, 0.52).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
