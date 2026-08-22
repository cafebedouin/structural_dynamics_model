% ============================================================================
% CONSTRAINT STORY: second_amendment_text__collective_security_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_text__collective_security_reading, []).

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
 *   constraint_id: second_amendment_text__collective_security_reading
 *   human_readable: Second Amendment — Collective-Security Reading (Militia-Conditioned Right)
 *   domain: constitutional/political
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the second_amendment_text kernel:
 *   the collective_security_reading, under which the prefatory militia clause
 *   conditions the operative right on organized civic defense and the state's
 *   police power may regulate private armament to serve collective security.
 *   The arrangement under contest — the epsilon referent, assessed by this
 *   reading's own lights — is the militia-conditioned regulatory order as it
 *   actually operated from the National Firearms Act era through the
 *   Heller/Bruen reversal: permit and licensing regimes, prohibited-person
 *   categories, and organized-defense privilege, with individual owners
 *   outside the organized channel bearing fees, delays, exclusions, and
 *   criminal exposure. By this reading's own lights most of that burden is
 *   the price of collective security; the structural data nonetheless show a
 *   genuine coordination function joined to asymmetric burden, which is why
 *   the claimed type is tangled_rope, authored independently of the metrics.
 *   The sibling readings (individual_right_reading,
 *   originalist_civic_virtue_reading) are separate constraint files with
 *   their own epsilon values and their own beneficiary/victim structures; the
 *   colloquial label 'Second Amendment' decomposes across them per the
 *   epsilon-invariance principle.
 *
 * KEY AGENTS:
 *   - state_regulatory_apparatus: primary beneficiary and agenda-setter (institutional/arbitrage) — writes and administers the permit regime, collects fees and gatekeeping authority
 *   - individual_gun_owners: primary target (organized/identity_locked) — bears fees, exclusions, and criminal exposure; ownership fused with identity
 *   - discretionary_denial_applicants: secondary target (powerless/trapped) — denied under administrator discretion without litigation resources
 *   - organized_militia_service_members: vindicated function-holders (organized/constrained) — occupy the organized-defense channel this reading privileges
 *   - high_violence_community_residents: intended coordination beneficiaries (moderate/constrained) — promised security, delivered unevenly
 *   - firearms_industry_manufacturers: dual-positioned regulated industry (powerful/arbitrage) — regulated and insulated at once
 *   - federal_appellate_courts: analytical observer (institutional/analytical) — adjudicated the reading's demotion
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_text__collective_security_reading, 0.45).
domain_priors:suppression_score(second_amendment_text__collective_security_reading, 0.47).
domain_priors:theater_ratio(second_amendment_text__collective_security_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_text__collective_security_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(second_amendment_text__collective_security_reading, suppression_requirement, 0.47).
narrative_ontology:constraint_metric(second_amendment_text__collective_security_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_text__collective_security_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(second_amendment_text__collective_security_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_text__collective_security_reading, tangled_rope).
narrative_ontology:human_readable(second_amendment_text__collective_security_reading, "Second Amendment — Collective-Security Reading (Militia-Conditioned Right)").
narrative_ontology:topic_domain(second_amendment_text__collective_security_reading, "constitutional/political").

domain_priors:requires_active_enforcement(second_amendment_text__collective_security_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_text__collective_security_reading, '3101b784-f331-4a13-ab42-8fc8272d87df').
narrative_ontology:cs_kernel_codification('3101b784-f331-4a13-ab42-8fc8272d87df', fixed_text).
narrative_ontology:cs_authority_grounding('3101b784-f331-4a13-ab42-8fc8272d87df', lineage).
narrative_ontology:cs_interpretation_layer_present('3101b784-f331-4a13-ab42-8fc8272d87df').
narrative_ontology:cs_reading_relation('3101b784-f331-4a13-ab42-8fc8272d87df', second_amendment_text__individual_right_reading, forecloses).
narrative_ontology:cs_reading_relation('3101b784-f331-4a13-ab42-8fc8272d87df', second_amendment_text__originalist_civic_virtue_reading, coexists_with).
narrative_ontology:cs_axiom('3101b784-f331-4a13-ab42-8fc8272d87df', foundational, militia_clause_conditions_operative_right).
narrative_ontology:cs_axiom_status(militia_clause_conditions_operative_right, holdable).
narrative_ontology:cs_axiom_grounding('3101b784-f331-4a13-ab42-8fc8272d87df', militia_clause_conditions_operative_right, conventional).
narrative_ontology:cs_axiom('3101b784-f331-4a13-ab42-8fc8272d87df', foundational, arms_regulation_legitimate_for_collective_security).
narrative_ontology:cs_axiom_status(arms_regulation_legitimate_for_collective_security, holdable).
narrative_ontology:cs_axiom_grounding('3101b784-f331-4a13-ab42-8fc8272d87df', arms_regulation_legitimate_for_collective_security, instrumental).
narrative_ontology:cs_reference_frame('3101b784-f331-4a13-ab42-8fc8272d87df', militia_conditioned_collective_security_order).
narrative_ontology:cs_drift_state('3101b784-f331-4a13-ab42-8fc8272d87df', post_heller_bruen_doctrine, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('3101b784-f331-4a13-ab42-8fc8272d87df', '').
narrative_ontology:cs_kernel_id(second_amendment_text__collective_security_reading, second_amendment_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_text__collective_security_reading, state_regulatory_apparatus).
narrative_ontology:constraint_beneficiary(second_amendment_text__collective_security_reading, organized_militia_service_members).
narrative_ontology:constraint_victim(second_amendment_text__collective_security_reading, individual_gun_owners).
narrative_ontology:constraint_victim(second_amendment_text__collective_security_reading, discretionary_denial_applicants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(second_amendment_text__collective_security_reading, high_violence_community_residents).
narrative_ontology:constraint_beneficiary(second_amendment_text__collective_security_reading, firearms_industry_manufacturers).
narrative_ontology:constraint_victim(second_amendment_text__collective_security_reading, firearms_industry_manufacturers).
narrative_ontology:constraint_vindicates(second_amendment_text__collective_security_reading, militia_clause_primacy).
narrative_ontology:constraint_vindicates(second_amendment_text__collective_security_reading, police_power_collective_security).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Legislatures, licensing agencies, and police departments that write and administer permit regimes, eligibility categories, and prohibited-person lists. Collects application fees, staffs administrative offices, and holds gatekeeping authority over who may possess what weaponry. Can reshape the regime's form — moving between may-issue and shall-issue designs, adding or pruning categories — while keeping the gatekeeping function itself intact.
narrative_ontology:constraint_stakeholder(second_amendment_text__collective_security_reading, state_regulatory_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).

% National Guard members and formally organized defense forces whose civic-defense role this reading vindicates; their armament runs through the constitutionally privileged channel. Bound by service obligations and command structure, their position depends on the organized-defense channel remaining the privileged one.
narrative_ontology:constraint_stakeholder(second_amendment_text__collective_security_reading, organized_militia_service_members, beneficiary,
    organized, biographical, constrained, national).

% Tens of millions who keep firearms for sport, collecting, and self-protection outside formal militia service. Pay fees, observe waiting periods, absorb eligibility exclusions, and face criminal exposure for noncompliance. For many, ownership is fused with regional culture and self-conception, so leaving would mean relinquishing a constitutive practice rather than swapping a tool. Politically organized through advocacy organizations and litigation.
narrative_ontology:constraint_stakeholder(second_amendment_text__collective_security_reading, individual_gun_owners, payer,
    organized, biographical, identity_locked, national).

% Applicants in may-issue jurisdictions whose permits turn on an administrator's judgment. Disproportionately working-class applicants without connections or the resources to litigate. Cannot obtain the license, cannot lawfully carry in the meantime, and generally cannot move jurisdictions where work and family are rooted.
narrative_ontology:constraint_stakeholder(second_amendment_text__collective_security_reading, discretionary_denial_applicants, payer,
    powerless, immediate, trapped, local).

% Residents of neighborhoods with elevated gun violence, for whom the reading's promise is fewer weapons in dangerous hands. Experience the costs of violence directly and also feel enforcement's burdens — stops, delays, denials — unevenly. Housing costs and social ties limit exit from affected areas.
narrative_ontology:constraint_stakeholder(second_amendment_text__collective_security_reading, high_violence_community_residents, beneficiary,
    moderate, immediate, constrained, local).

% Producers facing liability exposure, design restrictions, and marketing limits under the regime, while simultaneously insulated by licensing barriers that deter new entrants and by government procurement contracts. Can relocate production, pivot product lines, or shift sales across state lines when rules tighten in one place.
narrative_ontology:constraint_stakeholder(second_amendment_text__collective_security_reading, firearms_industry_manufacturers, payer,
    powerful, biographical, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(second_amendment_text__collective_security_reading, firearms_industry_manufacturers, beneficiary).

% Adjudicate which reading of the constitutional text governs. Produced the doctrinal sequence — Heller, McDonald, Bruen — that withdrew federal warrant from this reading's premises. Neither collects nor pays; shapes the arrangement's validity through precedent.
narrative_ontology:constraint_stakeholder(second_amendment_text__collective_security_reading, federal_appellate_courts, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(second_amendment_text__collective_security_reading, state_regulatory_apparatus).
narrative_ontology:fixing_cost_class(second_amendment_text__collective_security_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Centralizes decisions about who may possess what weaponry in accountable public institutions, replacing private arms-race escalation and vigilante self-help with organized, disciplined defense; makes armament a governed public function rather than a private default.
% TRANSFER_FUNCTION: Moves gatekeeping authority over armament from individuals to state institutions; moves compliance costs — fees, delays, training mandates, prohibited-status exclusions — onto individual owners and applicants; moves security provision from private self-help to publicly organized forces.
% ABSENT_VOICES: Historically, the people whom 'organized civic defense' itself excluded — enslaved and freed Black Americans disarmed by statute and custom, and later classes disparately denied under discretionary regimes — bore the arrangement's costs while its founding terms defined the militia without them. Today the unorganized owner speaks through litigation and elections, but the applicant denied by administrator discretion in a may-issue county rarely reaches any forum at all.
% DISAPPEARANCE_RATIONALE: If the militia-conditioned, regulation-permitting reading vanished overnight and the independent-individual-right reading fully governed, most licensing and permit regimes would fail heightened scrutiny, the administrative apparatus built on this reading would lose its constitutional warrant, and permitting schemes nationwide would dissolve or rebuild under far narrower authority; gun owners would move from regulated class to protected-class status.
% FOUNDING_PROBLEM: The 1791 arrangement answered the anti-federalist fear of standing armies: a well-regulated militia of the states' armed citizens as the necessary counterweight to professional federal military power, with the right secured so that such a militia could exist.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional historians across the interpretive spectrum — including originalist scholars who reject this reading's conclusions — attest that the ratification-era problem was the standing-army threat and that the National Guard's federal integration removed the state-militia counterweight the arrangement presupposed. No serious participant disputes the genealogy; the dispute is over what follows from it.
narrative_ontology:disappearance_verdict(second_amendment_text__collective_security_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_text__collective_security_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_text__collective_security_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(second_amendment_text__collective_security_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_text__collective_security_reading, 0.45, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_text__collective_security_reading_tests).
:- end_tests(second_amendment_text__collective_security_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Metrics are authored independently of the claim. Extractiveness 0.45 is reading-indexed over the standing arrangement: conditioning a liberty on state-defined purpose, monetized through fees and administered through discretion, falls unevenly on owners outside the organized channel — substantial, but by this reading's lights partly the price of collective security. Suppression 0.47 is a raw structural property and is not scaled by power or scope: criminal exposure for noncompliance is real, but lawful compliance paths, political channels, and geographic arbitrage persist. Theater_ratio 0.58 reflects the post-2008 condition: the reading's continuing operation is increasingly citational — rationales repeated in scholarship and some state doctrine after federal precedent withdrew warrant — while the apparatus it built still acts materially, keeping theater below piton levels. Accessibility_collapse 0.35: alternatives (comply, advocate, relocate, substitute means of protection) remain open once the arrangement is understood. Resistance 0.72: sustained litigation, electoral mobilization, and a doctrinal counter-revolution. The temporal series share one grid (1939-2026): extractiveness and suppression rise together through the enforcement buildout (GCA 1968, Brady, the 1994 ban), peak near 2000, and decline as Heller/McDonald/Bruen dismantle the reading's warrant, while theater rises monotonically as the justification outlives its authority — the signature of mandate succession rather than simple decay.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat the arrangement is ordinary governance: eligibility rules, fee schedules, public-safety administration. From the owner seat the same structure is gatekeeping over a constitutive practice, backed by criminal law. From the denied-applicant seat it is an arbitrary lottery. From the resident seat it is a promise that arrives unevenly. From the bench it was one reading among rivals until precedent demoted it. Same text, incompatible experiences — computed per-seat from power, exit, and role, not reconciled by the claim.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary/victim declarations drive the derivation. state_regulatory_apparatus and organized_militia_service_members sit near the beneficiary end (d roughly 0.05-0.15): the apparatus collects fees, budgets, and gatekeeping authority; militia members receive the privileged channel. individual_gun_owners and discretionary_denial_applicants sit near the target end (d roughly 0.85-0.95); identity_locked exit for owners and trapped exit for denied applicants push them toward the full-target pole. high_violence_community_residents sit near symmetric with a subsidy tilt (d roughly 0.35): promised protection, uneven delivery, no rents collected. firearms_industry_manufacturers are genuinely dual-positioned (d roughly 0.5): regulation costs them liability exposure while licensing barriers and procurement subsidize incumbency. No directionality_overrides are authored: the declarations plus exit atoms already produce these positions, and the override mechanism keys on power atoms, which would smear any correction across unrelated seats sharing a power level (the courts and the apparatus are both 'institutional'). Effective extraction is scaled by the engine from directionality and scope; suppression is not scaled.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — militias as counterweight to standing armies — is dead, corroborated across interpretive camps, yet the arrangement persists with a successor mandate (public safety). This is re-purposing, not pure inertia: the arrangement kept vitality by transferring its justification from the militia function to the police-power function. The classification guards against two errors. Reading the dead founding problem as proof of inertial theater (a piton verdict) would miss the material stakes — real prohibitions, real prosecutions, real denials. Reading the successor mandate as continuous with the founding one (a clean rope verdict) would launder the transfer. The mismatch between dead founding problem and world-rearranging persistence is itself the finding: the arrangement survived its mandate by changing what it is for, which is why theater_ratio climbs after 2008 while extractiveness stays material.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint is one reading of the second_amendment_text kernel — the collective_security_reading, instantiating the premise that the militia clause conditions the right on organized civic defense. Do the sibling readings (individual_right_reading: right independent of militia service with self-defense as core; originalist_civic_virtue_reading: universal armed citizenry as the militia) instantiate structurally different constraints, and where exactly does the disagreement bite?',
    'Doctrinal settlement — Heller, McDonald, and Bruen currently favor the individual-right sibling — together with continued state-court and scholarly practice; the disagreement is located in the function of the prefatory clause (condition versus amplification) and in whether personal self-defense counts as the protected core.',
    'If the individual-right sibling permanently prevails, this constraint''s beneficiary/victim structure inverts: the regulatory apparatus loses warrant, owners become a protected class, and this story''s epsilon referent shrinks to academic discourse and residual state doctrine.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Kernel-membership omega: this story is one of three readings of the Second Amendment text, with sibling-dependent structural deltas.').

omega_variable(
    founding_problem_succession,
    'Does the arrangement''s persistence after the death of its founding problem (the anti-standing-army militia guarantee) constitute legitimate succession to a new mandate (public safety), or inertial maintenance of a lapsed commitment?',
    'Trace the justification transfer historically: if courts and legislatures consciously re-grounded the arrangement in police-power public safety with independent warrant, it is succession; if it persists chiefly because amendment or abandonment is institutionally expensive, it is inertia.',
    'Succession supports the tangled_rope claim with a live coordination function; inertia pushes toward piton drift with rising theater and eventual decay.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_succession, conceptual, 'Whether the surviving arrangement is a re-founded commitment or a vestige wearing a new justification.').

omega_variable(
    regulatory_efficacy_empirics,
    'Does arms regulation actually produce the collective security that legitimizes it under this reading?',
    'Criminological quasi-experiments: shall-issue and permitless-carry adoptions, effects of permit regimes on homicide and suicide, cross-jurisdiction comparisons holding demographics constant.',
    'If regulation is efficacious, the coordination half is real and part of the measured burden is the price of coordination; if not, the coordination story thins toward cover and the structure drifts snare-ward.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_efficacy_empirics, empirical, 'Empirical foundation of the collective-security justification.').

omega_variable(
    discriminatory_enforcement_legacy,
    'Has enforcement under this reading operated discriminatorily — from Reconstruction-era disarmament statutes to disparate modern denials — such that the victim set is broader and differently composed than the formal classes name?',
    'Enforcement-data audits by race, class, and geography; archival work on historical disarmament statutes and their administration.',
    'Confirmed systematic discrimination raises the effective burden borne by excluded classes, expands the victim declaration beyond the generic owner class, and raises the suppression assessment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(discriminatory_enforcement_legacy, empirical, 'Whether the arrangement''s enforcement history targeted disfavored groups.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_text__collective_security_reading, 1939, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t1939, second_amendment_text__collective_security_reading, theater_ratio, 1939, 0.15).
narrative_ontology:measurement_basis(seco_tr_t1939, observed).
narrative_ontology:measurement(seco_tr_t1954, second_amendment_text__collective_security_reading, theater_ratio, 1954, 0.18).
narrative_ontology:measurement_basis(seco_tr_t1954, observed).
narrative_ontology:measurement(seco_tr_t1968, second_amendment_text__collective_security_reading, theater_ratio, 1968, 0.24).
narrative_ontology:measurement_basis(seco_tr_t1968, observed).
narrative_ontology:measurement(seco_tr_t1986, second_amendment_text__collective_security_reading, theater_ratio, 1986, 0.3).
narrative_ontology:measurement_basis(seco_tr_t1986, observed).
narrative_ontology:measurement(seco_tr_t2000, second_amendment_text__collective_security_reading, theater_ratio, 2000, 0.36).
narrative_ontology:measurement_basis(seco_tr_t2000, observed).
narrative_ontology:measurement(seco_tr_t2008, second_amendment_text__collective_security_reading, theater_ratio, 2008, 0.44).
narrative_ontology:measurement_basis(seco_tr_t2008, observed).
narrative_ontology:measurement(seco_tr_t2022, second_amendment_text__collective_security_reading, theater_ratio, 2022, 0.54).
narrative_ontology:measurement_basis(seco_tr_t2022, observed).
narrative_ontology:measurement(seco_tr_t2026, second_amendment_text__collective_security_reading, theater_ratio, 2026, 0.58).
narrative_ontology:measurement_basis(seco_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(seco_be_t1939, second_amendment_text__collective_security_reading, base_extractiveness, 1939, 0.28).
narrative_ontology:measurement_basis(seco_be_t1939, observed).
narrative_ontology:measurement(seco_be_t1954, second_amendment_text__collective_security_reading, base_extractiveness, 1954, 0.32).
narrative_ontology:measurement_basis(seco_be_t1954, observed).
narrative_ontology:measurement(seco_be_t1968, second_amendment_text__collective_security_reading, base_extractiveness, 1968, 0.43).
narrative_ontology:measurement_basis(seco_be_t1968, observed).
narrative_ontology:measurement(seco_be_t1986, second_amendment_text__collective_security_reading, base_extractiveness, 1986, 0.47).
narrative_ontology:measurement_basis(seco_be_t1986, observed).
narrative_ontology:measurement(seco_be_t2000, second_amendment_text__collective_security_reading, base_extractiveness, 2000, 0.53).
narrative_ontology:measurement_basis(seco_be_t2000, observed).
narrative_ontology:measurement(seco_be_t2008, second_amendment_text__collective_security_reading, base_extractiveness, 2008, 0.51).
narrative_ontology:measurement_basis(seco_be_t2008, observed).
narrative_ontology:measurement(seco_be_t2022, second_amendment_text__collective_security_reading, base_extractiveness, 2022, 0.46).
narrative_ontology:measurement_basis(seco_be_t2022, observed).
narrative_ontology:measurement(seco_be_t2026, second_amendment_text__collective_security_reading, base_extractiveness, 2026, 0.45).
narrative_ontology:measurement_basis(seco_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t1939, second_amendment_text__collective_security_reading, suppression_requirement, 1939, 0.35).
narrative_ontology:measurement_basis(seco_su_t1939, observed).
narrative_ontology:measurement(seco_su_t1954, second_amendment_text__collective_security_reading, suppression_requirement, 1954, 0.38).
narrative_ontology:measurement_basis(seco_su_t1954, observed).
narrative_ontology:measurement(seco_su_t1968, second_amendment_text__collective_security_reading, suppression_requirement, 1968, 0.48).
narrative_ontology:measurement_basis(seco_su_t1968, observed).
narrative_ontology:measurement(seco_su_t1986, second_amendment_text__collective_security_reading, suppression_requirement, 1986, 0.52).
narrative_ontology:measurement_basis(seco_su_t1986, observed).
narrative_ontology:measurement(seco_su_t2000, second_amendment_text__collective_security_reading, suppression_requirement, 2000, 0.6).
narrative_ontology:measurement_basis(seco_su_t2000, observed).
narrative_ontology:measurement(seco_su_t2008, second_amendment_text__collective_security_reading, suppression_requirement, 2008, 0.58).
narrative_ontology:measurement_basis(seco_su_t2008, observed).
narrative_ontology:measurement(seco_su_t2022, second_amendment_text__collective_security_reading, suppression_requirement, 2022, 0.5).
narrative_ontology:measurement_basis(seco_su_t2022, observed).
narrative_ontology:measurement(seco_su_t2026, second_amendment_text__collective_security_reading, suppression_requirement, 2026, 0.47).
narrative_ontology:measurement_basis(seco_su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_text__collective_security_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(second_amendment_text__collective_security_reading, second_amendment_text__individual_right_reading).
narrative_ontology:affects_constraint(second_amendment_text__collective_security_reading, second_amendment_text__originalist_civic_virtue_reading).

% DUAL FORMULATION NOTE:
% 'The Second Amendment' is a colloquial label covering structurally distinct claims; per the epsilon-invariance principle it decomposes into three linked files — this collective-security reading (state apparatus as beneficiary, owners as constrained class), the individual-right reading (owners as protected class, regulators as constrained), and the originalist civic-virtue reading (citizen-soldier capacity as the protected object). Epsilon differs across the family because the arrangements differ, not because one constraint is measured differently. Doctrinal history runs upstream-downstream: the Miller-era collective frame shaped the legal environment to which the individual-right reaction (Heller/Bruen) responded.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
