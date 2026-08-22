% ============================================================================
% CONSTRAINT STORY: dignified_death__sanctity_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:constraint_vindicates/2,
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
 *   human_readable: Sanctity-of-Life Prohibition on Intentional Life-Termination (Dignified-Death Kernel, Sanctity Reading)
 *   domain: bioethics/medical law/political philosophy
 *
 * SUMMARY:
 *   In most jurisdictions the standing end-of-life arrangement forbids
 *   intentional life-termination outright: no consent, no procedural
 *   safeguard, no suffering threshold unlocks it. The arrangement is
 *   justified as the protection of life's intrinsic value and of vulnerable
 *   people from pressure toward death; it is carried by criminal statute,
 *   professional licensure, and religious-moral authority, and it binds
 *   everyone — but its costs land regressively, on those least able to
 *   purchase exits abroad or informal assistance. KEY AGENTS (by structural
 *   relationship): - terminally_ill_seeking_assisted_dying /
 *   elderly_patients_under_managed_dying /
 *   poor_patients_without_exit_options: primary targets (powerless/trapped) —
 *   bear prolonged dying at full force -
 *   disabled_and_chronically_ill_patients: contested seat
 *   (moderate/constrained) — simultaneously defended by and denied by the
 *   same rule - religious_institutions and faith_communities: primary
 *   beneficiaries (institutional, organized / identity_locked) — collect
 *   authority and cohesion - palliative_care_movement: incidental beneficiary
 *   (organized/mobile) - physicians_and_nurses: dual-positioned payers
 *   (organized/constrained) — bear criminal exposure and moral distress, gain
 *   a bright liability line - legislators_and_courts,
 *   medical_licensing_bodies: agenda setters (institutional/constrained) —
 *   administer and enforce - family_caregivers_of_the_suffering: excluded
 *   seat (moderate/trapped) — structurally silenced -
 *   comparative_bioethics_analysts: analytical observers — see the full
 *   cross-jurisdictional structure The claim/metric gap is deliberate and is
 *   the datum: the reading CLAIMS the norm is transcendent moral law
 *   (mountain rhetoric), while the authored metrics describe enforced,
 *   victim-bearing operation. The engine measures that divergence; this story
 *   does not reconcile them.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dignified_death__sanctity_primary, 0.58).
domain_priors:suppression_score(dignified_death__sanctity_primary, 0.64).
domain_priors:theater_ratio(dignified_death__sanctity_primary, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dignified_death__sanctity_primary, extractiveness, 0.58).
narrative_ontology:constraint_metric(dignified_death__sanctity_primary, suppression_requirement, 0.64).
narrative_ontology:constraint_metric(dignified_death__sanctity_primary, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dignified_death__sanctity_primary, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(dignified_death__sanctity_primary, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dignified_death__sanctity_primary, snare).
narrative_ontology:human_readable(dignified_death__sanctity_primary, "Sanctity-of-Life Prohibition on Intentional Life-Termination (Dignified-Death Kernel, Sanctity Reading)").
narrative_ontology:topic_domain(dignified_death__sanctity_primary, "bioethics/medical law/political philosophy").

domain_priors:requires_active_enforcement(dignified_death__sanctity_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dignified_death__sanctity_primary, '09f8e951-cfda-4d86-934a-a5f7e9d160c1').
narrative_ontology:cs_kernel_codification('09f8e951-cfda-4d86-934a-a5f7e9d160c1', formalized).
narrative_ontology:cs_authority_grounding('09f8e951-cfda-4d86-934a-a5f7e9d160c1', lineage).
narrative_ontology:cs_interpretation_layer_present('09f8e951-cfda-4d86-934a-a5f7e9d160c1').
narrative_ontology:cs_reading_relation('09f8e951-cfda-4d86-934a-a5f7e9d160c1', dignified_death__autonomy_primary, forecloses).
narrative_ontology:cs_reading_relation('09f8e951-cfda-4d86-934a-a5f7e9d160c1', dignified_death__relational_autonomy, forecloses).
narrative_ontology:cs_axiom('09f8e951-cfda-4d86-934a-a5f7e9d160c1', foundational, life_holds_intrinsic_inviolable_value).
narrative_ontology:cs_axiom_status(life_holds_intrinsic_inviolable_value, holdable).
narrative_ontology:cs_axiom_grounding('09f8e951-cfda-4d86-934a-a5f7e9d160c1', life_holds_intrinsic_inviolable_value, deontological).
narrative_ontology:cs_axiom('09f8e951-cfda-4d86-934a-a5f7e9d160c1', foundational, consent_cannot_waive_transcendent_law).
narrative_ontology:cs_axiom_status(consent_cannot_waive_transcendent_law, holdable).
narrative_ontology:cs_axiom_grounding('09f8e951-cfda-4d86-934a-a5f7e9d160c1', consent_cannot_waive_transcendent_law, deontological).
narrative_ontology:cs_axiom('09f8e951-cfda-4d86-934a-a5f7e9d160c1', secondary, killing_letting_die_moral_asymmetry).
narrative_ontology:cs_axiom_status(killing_letting_die_moral_asymmetry, holdable).
narrative_ontology:cs_axiom_grounding('09f8e951-cfda-4d86-934a-a5f7e9d160c1', killing_letting_die_moral_asymmetry, deontological).
narrative_ontology:cs_reference_frame('09f8e951-cfda-4d86-934a-a5f7e9d160c1', transcendent_sanctity_of_life).
narrative_ontology:cs_drift_state('09f8e951-cfda-4d86-934a-a5f7e9d160c1', contemporary_pluralistic_jurisprudence, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('09f8e951-cfda-4d86-934a-a5f7e9d160c1', '').
narrative_ontology:cs_kernel_id(dignified_death__sanctity_primary, dignified_death).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dignified_death__sanctity_primary, religious_institutions).
narrative_ontology:constraint_beneficiary(dignified_death__sanctity_primary, faith_communities).
narrative_ontology:constraint_beneficiary(dignified_death__sanctity_primary, palliative_care_movement).
narrative_ontology:constraint_victim(dignified_death__sanctity_primary, terminally_ill_seeking_assisted_dying).
narrative_ontology:constraint_victim(dignified_death__sanctity_primary, elderly_patients_under_managed_dying).
narrative_ontology:constraint_victim(dignified_death__sanctity_primary, disabled_and_chronically_ill_patients).
narrative_ontology:constraint_victim(dignified_death__sanctity_primary, poor_patients_without_exit_options).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(dignified_death__sanctity_primary, disabled_and_chronically_ill_patients).
narrative_ontology:constraint_beneficiary(dignified_death__sanctity_primary, physicians_and_nurses).
narrative_ontology:constraint_victim(dignified_death__sanctity_primary, physicians_and_nurses).
narrative_ontology:constraint_vindicates(dignified_death__sanctity_primary, sanctity_of_life_doctrine).
narrative_ontology:constraint_vindicates(dignified_death__sanctity_primary, intrinsic_dignity_of_life).
narrative_ontology:constraint_vindicates(dignified_death__sanctity_primary, divine_sovereignty_over_life).
narrative_ontology:constraint_vindicates(dignified_death__sanctity_primary, killing_letting_die_asymmetry).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Teach that human life is held in trust from God and may not be intentionally ended. Operate hospitals, bioethics institutes, and lobbying arms that oppose assisted-dying legislation; file amicus briefs; convene commissions that define permissible end-of-life practice. When statute tracks the teaching, the institutions' moral authority and institutional reach grow. Leaving the teaching would dissolve the communities' reason for cohesion, so departure is not a live option from inside.
narrative_ontology:constraint_stakeholder(dignified_death__sanctity_primary, religious_institutions, beneficiary,
    institutional, civilizational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(dignified_death__sanctity_primary, religious_institutions, agenda_setter).

% Lay members who organize around reverence-for-life norms: volunteer presence at bedsides, funeral rites, and a moral vocabulary that frames endurance of suffering as meaningful rather than meaningless. They supply much of the electoral weight legislators weigh on this issue. Their belonging is bound up with the norm, so abandoning it would cost identity, not merely opinion.
narrative_ontology:constraint_stakeholder(dignified_death__sanctity_primary, faith_communities, beneficiary,
    organized, generational, identity_locked, global).

% Hospices and palliative services that receive the patients the prohibition channels away from hastened death. Funding streams, endowed chairs, and public mandates have grown alongside the ban under the banner of answering suffering with care rather than killing. The movement could operate under any legal regime and already serves permissive jurisdictions, so its attachment to the prohibition is professional and moral rather than structural.
narrative_ontology:constraint_stakeholder(dignified_death__sanctity_primary, palliative_care_movement, beneficiary,
    organized, generational, mobile, continental).

% Diagnosed with progressive incurable illness, they petition physicians and courts for help ending life on their own schedule. The law answers no; physicians risk license and prosecution; travel to permissive jurisdictions costs thousands and requires physical capacity many no longer have; underground help exists but is unreliable and criminal. Most remain to the biological end, managing symptoms as best palliation allows.
narrative_ontology:constraint_stakeholder(dignified_death__sanctity_primary, terminally_ill_seeking_assisted_dying, payer,
    powerless, immediate, trapped, national).

% Older adults in decline whose families, care institutions, and clinicians manage the pace of their dying. With no negotiated ending available, continuance is the default regardless of preference. Those dependent on caregivers or institutions feel the weight of expectations to persevere; those with savings sometimes purchase exits abroad that their poorer contemporaries cannot.
narrative_ontology:constraint_stakeholder(dignified_death__sanctity_primary, elderly_patients_under_managed_dying, payer,
    powerless, biographical, trapped, national).

% People living long-term with disability or chronic illness. Advocacy organizations among them split sharply: some campaign against assisted-dying laws as vectors of ableist pressure and defend the prohibition as their shield; others demand access as a matter of equal self-authority. Individually they hold little leverage over the rule either way; their practical options track income and mobility like everyone else's.
narrative_ontology:constraint_stakeholder(dignified_death__sanctity_primary, disabled_and_chronically_ill_patients, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(dignified_death__sanctity_primary, disabled_and_chronically_ill_patients, beneficiary).

% Low-income patients whose suffering lasts longest under managed dying: no foreign clinic, no private physician willing to risk a license, no attorney. They receive whatever the public system offers and endure. The rule binds them at full strength while wealthier counterparts buy partial exemptions.
narrative_ontology:constraint_stakeholder(dignified_death__sanctity_primary, poor_patients_without_exit_options, payer,
    powerless, immediate, trapped, regional).

% Clinicians who absorb the rule at the bedside: they may withdraw treatment and titrate opioids under double-effect reasoning but may not act with intent to end life, on pain of license revocation and prosecution. Professional associations defend the line as protecting patient trust and shielding clinicians from escalating demands; many practitioners privately report moral distress when patients beg for release. Emigration to permissive systems remains open to the credentialed and mobile.
narrative_ontology:constraint_stakeholder(dignified_death__sanctity_primary, physicians_and_nurses, payer,
    organized, biographical, constrained, continental).
narrative_ontology:stakeholder_secondary_role(dignified_death__sanctity_primary, physicians_and_nurses, beneficiary).

% Maintain the criminal statutes and constitutional interpretations that carry the rule. Authorization bills recur and fail under organized opposition; courts occasionally force accommodations around withdrawal and terminal sedation while stopping short of permitting intentional termination. Touching the issue carries concentrated career risk, so deferral is the consistently chosen move.
narrative_ontology:constraint_stakeholder(dignified_death__sanctity_primary, legislators_and_courts, agenda_setter,
    institutional, generational, constrained, national).

% Discipline clinicians who cross the line, publish guidance distinguishing permissible palliation from forbidden intent, and investigate complaints. Their guidance is where the rule lives day to day; their disciplinary caseload stays thin because anticipatory compliance does most of the work.
narrative_ontology:constraint_stakeholder(dignified_death__sanctity_primary, medical_licensing_bodies, agenda_setter,
    institutional, biographical, constrained, national).

% Spouses and children providing years of hands-on care whose exhaustion has no legitimate voice: advocating for the patient's death would read as self-interest, so they perform devotion publicly while privately breaking down. Hearings on the rule seat clergy, physicians, and disability advocates; caregivers testify rarely and usually anonymously.
narrative_ontology:constraint_stakeholder(dignified_death__sanctity_primary, family_caregivers_of_the_suffering, excluded,
    moderate, immediate, trapped, local).

% Scholars and commissions comparing jurisdictions: they document how permissive and prohibitive regimes differ in coercion rates, palliation access, and public trust, and publish the record that both sides cite. They hold no vote; their analyses enter the contest as ammunition.
narrative_ontology:constraint_stakeholder(dignified_death__sanctity_primary, comparative_bioethics_analysts, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dignified_death__sanctity_primary, religious_institutions).
narrative_ontology:fixing_cost_class(dignified_death__sanctity_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains one bright line between permissible end-of-life practice (symptom relief, treatment withdrawal) and intentional killing, so that medicine's lethal potential stays governed by a shared standard and no patient, family, or clinician must adjudicate it alone; the line also gives vulnerable people a public norm to stand behind when pressed toward death.
% TRANSFER_FUNCTION: Moves decision authority over the timing of death from suffering individuals and their bedside teams to the moral-legal order (statute, court, licensing body, church); moves the resulting costs — prolonged dying, criminal exposure, moral distress — onto patients and clinicians; moves authority and reassurance upward to the institutions that administer the line.
% ABSENT_VOICES: Exhausted family caregivers, whose voice would read as self-interest and so stays silent or anonymous; non-terminal chronic sufferers whose requests fall outside every eligibility frame; poor patients without counsel or mobility. They sit outside hearing rooms dominated by religious bodies, medical associations, and disability organizations.
% DISAPPEARANCE_RATIONALE: Overnight repeal would trigger immediate reorganization: eligibility statutes drafted, licensing guidance rewritten, palliative-funding coalitions renegotiated, religious institutions losing their central bioethics battleground, and clinical practice splitting between permissive and conscientious-objector tracks within months.
% FOUNDING_PROBLEM: Prevent the return of involuntary killing and eugenic disposal: the twentieth century's forced sterilizations and 'life unworthy of life' programs taught drafters that any licensed power over death migrates toward the powerless; the categorical ban was built so that no committee, court, or budget could ever weigh a life against its cost.
% FOUNDING_PROBLEM_CORROBORATION: Disability-rights organizations opposing legalization — outside the religious beneficiary set — attest the vulnerability problem is live, citing coercion findings in permissive jurisdictions; empirical compliance research from Oregon and the Benelux countries documents both functioning safeguards and boundary erosion. No serious party denies the founding problem existed; the contest is whether it still requires the categorical, consent-blind form.
narrative_ontology:disappearance_verdict(dignified_death__sanctity_primary, world_rearranges).
narrative_ontology:founding_problem_status(dignified_death__sanctity_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dignified_death__sanctity_primary, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(dignified_death__sanctity_primary, 'none', 1).
narrative_ontology:epsilon_provenance(dignified_death__sanctity_primary, 0.58, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness ends the interval at 0.58 (within the manifest's 0.50-0.65 band): the denial is universal and its burden is regressive — the mobile and wealthy buy partial exemptions (foreign clinics, sympathetic private physicians) while the trapped poor and institutionalized endure the whole arrangement. Suppression is 0.64 as a raw, unscaled structural property: persistence depends on criminal statute and licensure discipline, not participant preference. Theater_ratio 0.38: enforcement caseloads are thin because anticipation does the work, and a growing share of maintenance is declarative (encyclicals, 'dignity' invocations, ethics-board pronouncements) rather than functional. Accessibility_collapse 0.50: alternatives persist but are priced — palliative sedation substitutes partially, travel and gray markets serve the few. Resistance 0.68: sixty years of continuous litigation, referendum campaigns, and legislative attempts. emerges_naturally is false: however transcendentally framed, the norm's content and enforcement track religious affiliation and institutional interest, which is constructed-norm behavior. The measurement series run on one shared time grid (seven points, every tracked metric at every point). The suppression series is authored deliberately: enforcement was heavy mid-century, relaxed during the decriminalization-of-suicide wave, then ratcheted back up from the 1990s (new assisted-suicide felonies, board discipline) as reform pressure mounted — the story tracks enforcement-capacity change, so suppression_requirement is the right series. Extractiveness climbs monotonically with technology: intensive-care medicine made 'letting nature take its course' into indefinite managed prolongation, sharpening the rule's bite without changing its text.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute very differently. From the sanctuary seat (religious institutions, faith communities) the arrangement is covenant: a subsidy of meaning and authority, near-zero felt cost. From the trapped patient seats it is a wall: full-price denial with no exit. From the clinician seat it is a liability line that protects as it constrains. From the disability-advocacy seat it is simultaneously shield and cage — the same rule is defended and attacked by people in the same structural position, differentiated by ideology rather than power. The engine computes per-seat classifications from the power/exit atoms and role declarations; this divergence is expected output, not an authoring inconsistency.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations place religious_institutions and faith_communities near the subsidized pole: they collect authority, cohesion, and institutional reach, and their identity_locked exit means the arrangement costs them nothing they would accept losing. palliative_care_movement sits near-symmetric with a mild beneficiary tilt: real service revenue and mandate, but regime-portable. The four victim declarations place the patient groups near the target pole, and their trapped/constrained exits push them toward the full-target end — trapped targets sit nearer d=1.0 than mobile ones, which is exactly the regressive structure described above. physicians_and_nurses derive a mid-to-high d from their payer role, moderated by their beneficiary secondary role (the bright line shields them). legislators_and_courts and medical_licensing_bodies carry agenda-setter positions with mixed exposure. No directionality_overrides are authored: the derivation chain from beneficiary/victim declarations plus exit atoms produces the right relationships, and the one genuinely ambiguous seat (disabled_and_chronically_ill_patients, dual-positioned) is handled by its secondary_role declaration plus the dedicated omega rather than a numeric patch.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — preventing involuntary killing and eugenic disposal — is not dead: coercion findings in permissive jurisdictions and organized disability opposition attest it is live. Accordingly founding_problem_status is 'contested' paired with disappearance_verdict 'world_rearranges', which does not trip the dead-mandate/zombie mismatch flag. The classification guards against two opposite mislabels. First, accepting the reading's self-description at face value would render the arrangement a pure protection norm (rope-like) and erase the regressive burden, the suppressed exits, and the consent-blind absoluteness that make enforcement doing the holding. Second, declaring the mandate obsolete (piton-like) would ignore that the vulnerability problem is empirically live wherever the categorical form is lifted. mandatrophy_resolved is therefore not set: the mandate has migrated rather than died, and whether the categorical form — as opposed to narrower procedural protection — is still required is precisely what the omegas route to investigation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_structure,
    'This story instantiates one reading (sanctity_primary) of the contested kernel dignified_death; what would the sibling readings change structurally?',
    'Compile the sibling stories (dignified_death__autonomy_primary, dignified_death__relational_autonomy) and compare: under autonomy_primary the prohibition''s denial-victims vanish and legalization''s coercion risks appear as the victim set; under relational_autonomy enforcement redistributes across the triad and the categorical form dissolves into procedure. Epsilon reindexes over the same shared referent in each.',
    'If a sibling reading became governing law, this constraint''s victim set empties into the sibling''s and classification recomputes from the sibling''s structural data. The disagreement is located in where dignity resides and whether consent can waive the protection — not in the facts of suffering.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Committer frame: one of three readings of the dignified_death kernel; sibling readings flip the victim set and redistribute enforcement.').

omega_variable(
    protection_genuine_or_cover,
    'Is the vulnerable-population protection function a genuine coordination output of the categorical prohibition, or cover for enforcing a moral absolute?',
    'Compare coercion-of-the-vulnerable rates and palliation access across prohibitive versus procedurally-guarded permissive jurisdictions; if narrow safeguarded regimes achieve equivalent protection, the categorical consent-blind form is doing no protection work the procedure could not.',
    'Genuine protection would push the structure toward hybrid coordination-plus-extraction; cover would confirm the pure-coercion reading and harden the snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(protection_genuine_or_cover, empirical, 'Whether the protection story is load-bearing or rhetorical.').

omega_variable(
    vulnerable_seat_directionality,
    'Are elderly, disabled, and poor patients net bearers of the prohibition''s costs, or net protectees as the reading claims?',
    'Disaggregate harm and revealed-preference data by age, disability status, and income under both regime types; note that disabled populations themselves split today, so survey within-group preference distributions rather than treating the class as unitary.',
    'If protectees dominate, the vulnerable seats shift toward the subsidized pole and the overall classification softens toward hybrid; if bearers dominate, the declared victim set stands at full weight and the regressive structure is confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(vulnerable_seat_directionality, empirical, 'Direction of the vulnerable populations'' true structural position.').

omega_variable(
    transcendent_law_vs_constructed_norm,
    'Is the prohibition a discovered transcendent moral law, as the reading claims, or a constructed norm maintained by enforcement?',
    'Cross-cultural convergence test: if the norm''s content and enforcement intensity vary systematically with religious affiliation, colonial history, and institutional interest, it behaves as constructed; convergence independent of those variables would support transcendent status.',
    'Transcendent status would recast the measured costs as the price of conforming to reality rather than of enforcement, collapsing the extraction framing; constructed status leaves the coercive reading intact.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(transcendent_law_vs_constructed_norm, conceptual, 'Naturalness contest over the norm''s source — the reading''s own mountain-rhetoric made testable.').

omega_variable(
    internalized_duty_to_endure,
    'How much of the observed acquiescence to prolonged dying is internalized duty rather than agreement?',
    'Post-reform request-rate jumps in permissive jurisdictions reveal suppressed demand that prohibition-era surveys and clinical records undercount; compare pre/post request volumes matched by diagnosis and prognosis.',
    'Higher internalization means measured consent to suffering understates coercion: the effective suppressive force exceeds the structural measure because patients carry the duty with them and stop asking.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_duty_to_endure, empirical, 'Internalized component of suppression in the patient seats.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dignified_death__sanctity_primary, 1960, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dd_sp_tr_t1960, dignified_death__sanctity_primary, theater_ratio, 1960, 0.14).
narrative_ontology:measurement(dd_sp_tr_t1975, dignified_death__sanctity_primary, theater_ratio, 1975, 0.17).
narrative_ontology:measurement(dd_sp_tr_t1985, dignified_death__sanctity_primary, theater_ratio, 1985, 0.2).
narrative_ontology:measurement(dd_sp_tr_t1995, dignified_death__sanctity_primary, theater_ratio, 1995, 0.24).
narrative_ontology:measurement(dd_sp_tr_t2005, dignified_death__sanctity_primary, theater_ratio, 2005, 0.28).
narrative_ontology:measurement(dd_sp_tr_t2015, dignified_death__sanctity_primary, theater_ratio, 2015, 0.33).
narrative_ontology:measurement(dd_sp_tr_t2025, dignified_death__sanctity_primary, theater_ratio, 2025, 0.38).

% Extraction over time
narrative_ontology:measurement(dd_sp_be_t1960, dignified_death__sanctity_primary, base_extractiveness, 1960, 0.34).
narrative_ontology:measurement(dd_sp_be_t1975, dignified_death__sanctity_primary, base_extractiveness, 1975, 0.38).
narrative_ontology:measurement(dd_sp_be_t1985, dignified_death__sanctity_primary, base_extractiveness, 1985, 0.42).
narrative_ontology:measurement(dd_sp_be_t1995, dignified_death__sanctity_primary, base_extractiveness, 1995, 0.46).
narrative_ontology:measurement(dd_sp_be_t2005, dignified_death__sanctity_primary, base_extractiveness, 2005, 0.51).
narrative_ontology:measurement(dd_sp_be_t2015, dignified_death__sanctity_primary, base_extractiveness, 2015, 0.55).
narrative_ontology:measurement(dd_sp_be_t2025, dignified_death__sanctity_primary, base_extractiveness, 2025, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(dd_sp_su_t1960, dignified_death__sanctity_primary, suppression_requirement, 1960, 0.56).
narrative_ontology:measurement(dd_sp_su_t1975, dignified_death__sanctity_primary, suppression_requirement, 1975, 0.5).
narrative_ontology:measurement(dd_sp_su_t1985, dignified_death__sanctity_primary, suppression_requirement, 1985, 0.47).
narrative_ontology:measurement(dd_sp_su_t1995, dignified_death__sanctity_primary, suppression_requirement, 1995, 0.53).
narrative_ontology:measurement(dd_sp_su_t2005, dignified_death__sanctity_primary, suppression_requirement, 2005, 0.57).
narrative_ontology:measurement(dd_sp_su_t2015, dignified_death__sanctity_primary, suppression_requirement, 2015, 0.61).
narrative_ontology:measurement(dd_sp_su_t2025, dignified_death__sanctity_primary, suppression_requirement, 2025, 0.64).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dignified_death__sanctity_primary, identity_coordination).
narrative_ontology:affects_constraint(dignified_death__sanctity_primary, dignified_death__autonomy_primary).
narrative_ontology:affects_constraint(dignified_death__sanctity_primary, dignified_death__relational_autonomy).

% DUAL FORMULATION NOTE:
% The colloquial label 'dignity in dying' decomposes into three structurally distinct constraints — one per reading of the dignified_death kernel: this sanctity-primary prohibition (consent-blind ban, victims among the trapped and poor), the autonomy-primary permission regime (final authority with the sufferer), and the relational-autonomy procedural regime (distributed authority behind safeguards). Each carries its own epsilon, victim set, and classification; they are linked here as a constraint family per the epsilon-invariance principle. Direction of influence: the sanctity reading is the historical baseline from which the siblings diverge, and its enforcement record supplies the evidence both siblings argue from.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
