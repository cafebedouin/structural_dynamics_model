% ============================================================================
% CONSTRAINT STORY: second_amendment_arms_right__individual_right_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_arms_right__individual_right_reading, []).

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
 *   constraint_id: second_amendment_arms_right__individual_right_reading
 *   human_readable: Second Amendment Individual-Right Reading: Pre-Existing Personal Liberty Protected Against Federal Infringement
 *   domain: legal/constitutional/political_philosophy
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested Second Amendment
 *   kernel: the individual-right reading, under which keeping and bearing
 *   arms is a personal liberty that predates government and binds the federal
 *   government (and, since McDonald, the states). The standing arrangement
 *   under contest is the operative doctrine from Heller (2008) through Bruen
 *   (2022) and its aftermath. Sibling readings, collective-right and
 *   civic-republican, are separate constraints with different epsilon: the
 *   collective reading would name state militias as the protected party and
 *   individual owners as neither beneficiary nor target; the civic-republican
 *   reading centers armed citizenship as a duty of self-governance. Those
 *   differences live in the siblings' files and in the omega variables here,
 *   not inside this constraint. Claim/metric independence is deliberate: the
 *   reading's adherents claim a rope-like shield around a pre-existing
 *   liberty, while the authored metrics describe a structure with a genuine
 *   coordination core AND asymmetric cost-bearing, actively enforced by
 *   courts. The engine adjudicates that divergence; this file does not
 *   reconcile it.
 *
 * KEY AGENTS:
 *   - - individual_gun_owners: Primary beneficiary (organized/identity_locked) — protected class; ownership fused with cultural identity
 *   - - firearms_industry: Secondary beneficiary with residual payer position (powerful/arbitrage) — collects commercial upside under the protection umbrella while answering to remaining federal oversight
 *   - - gun_rights_advocacy_organizations: Beneficiary (organized/identity_locked) — institutional existence bound to the guarantee
 *   - - firearms_regulatory_authorities: Primary target (institutional/trapped) — lose regulatory instruments with each doctrinal expansion
 *   - - gun_violence_exposed_communities: Diffuse cost-bearers (moderate/trapped) — carry the public-safety externality without a seat in the interpretive forum
 *   - - supreme_court: Agenda-setter (institutional/mobile) — defines the reading's content and could reverse it
 *   - - gun_control_advocacy_movements: Excluded voice (organized/constrained) — methodologically barred from the adjudicative conversation
 *   - - constitutional_law_academy: Analytical observer (analytical/analytical) — maps the doctrine's coherence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_arms_right__individual_right_reading, 0.58).
domain_priors:suppression_score(second_amendment_arms_right__individual_right_reading, 0.55).
domain_priors:theater_ratio(second_amendment_arms_right__individual_right_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_arms_right__individual_right_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(second_amendment_arms_right__individual_right_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(second_amendment_arms_right__individual_right_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_arms_right__individual_right_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(second_amendment_arms_right__individual_right_reading, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_arms_right__individual_right_reading, tangled_rope).
narrative_ontology:human_readable(second_amendment_arms_right__individual_right_reading, "Second Amendment Individual-Right Reading: Pre-Existing Personal Liberty Protected Against Federal Infringement").
narrative_ontology:topic_domain(second_amendment_arms_right__individual_right_reading, "legal/constitutional/political_philosophy").

domain_priors:requires_active_enforcement(second_amendment_arms_right__individual_right_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_arms_right__individual_right_reading, 'b01d33a1-0eff-436a-a34c-6aa46e4edb7f').
narrative_ontology:cs_kernel_codification('b01d33a1-0eff-436a-a34c-6aa46e4edb7f', fixed_text).
narrative_ontology:cs_authority_grounding('b01d33a1-0eff-436a-a34c-6aa46e4edb7f', lineage).
narrative_ontology:cs_interpretation_layer_present('b01d33a1-0eff-436a-a34c-6aa46e4edb7f').
narrative_ontology:cs_reading_relation('b01d33a1-0eff-436a-a34c-6aa46e4edb7f', second_amendment_arms_right__collective_right_reading, forecloses).
narrative_ontology:cs_reading_relation('b01d33a1-0eff-436a-a34c-6aa46e4edb7f', second_amendment_arms_right__civic_republican_reading, coexists_with).
narrative_ontology:cs_axiom('b01d33a1-0eff-436a-a34c-6aa46e4edb7f', foundational, arms_right_preexists_government).
narrative_ontology:cs_axiom_status(arms_right_preexists_government, holdable).
narrative_ontology:cs_axiom_grounding('b01d33a1-0eff-436a-a34c-6aa46e4edb7f', arms_right_preexists_government, deontological).
narrative_ontology:cs_axiom('b01d33a1-0eff-436a-a34c-6aa46e4edb7f', secondary, founding_era_tradition_bounds_meaning).
narrative_ontology:cs_axiom_status(founding_era_tradition_bounds_meaning, holdable).
narrative_ontology:cs_axiom_grounding('b01d33a1-0eff-436a-a34c-6aa46e4edb7f', founding_era_tradition_bounds_meaning, conventional).
narrative_ontology:cs_reference_frame('b01d33a1-0eff-436a-a34c-6aa46e4edb7f', preexisting_personal_liberty_acknowledgment).
narrative_ontology:cs_drift_state('b01d33a1-0eff-436a-a34c-6aa46e4edb7f', post_bruen_expansion_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('b01d33a1-0eff-436a-a34c-6aa46e4edb7f', '').
narrative_ontology:cs_kernel_id(second_amendment_arms_right__individual_right_reading, second_amendment_arms_right).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_arms_right__individual_right_reading, individual_gun_owners).
narrative_ontology:constraint_beneficiary(second_amendment_arms_right__individual_right_reading, firearms_industry).
narrative_ontology:constraint_beneficiary(second_amendment_arms_right__individual_right_reading, gun_rights_advocacy_organizations).
narrative_ontology:constraint_victim(second_amendment_arms_right__individual_right_reading, firearms_regulatory_authorities).
narrative_ontology:constraint_victim(second_amendment_arms_right__individual_right_reading, gun_violence_exposed_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(second_amendment_arms_right__individual_right_reading, firearms_industry).
narrative_ontology:constraint_victim(second_amendment_arms_right__individual_right_reading, gun_control_advocacy_movements).
narrative_ontology:constraint_vindicates(second_amendment_arms_right__individual_right_reading, preexisting_natural_right_doctrine).
narrative_ontology:constraint_vindicates(second_amendment_arms_right__individual_right_reading, text_history_tradition_method).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Own firearms for self-defense, hunting, sport, and collection. The constitutional guarantee secures their ability to acquire, keep, and carry arms free of federal prohibition, and courts increasingly strike down state and local restrictions on their behalf. Legally, selling out and ceasing ownership is simple; culturally, for the core constituency ownership is woven into rural identity, self-reliance ideals, and political affiliation, so departure carries social and self-concept costs most never contemplate.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__individual_right_reading, individual_gun_owners, beneficiary,
    organized, biographical, identity_locked, national).

% Manufactures and sells firearms and ammunition. The guarantee stabilizes and expands its domestic market: carry expansions widen the customer base, and the constitutional shield discourages prohibition-style regulation of its products. It simultaneously lives under federal dealer licensing, trace, and import rules administered by ATF, so it operates both inside the protection and under residual federal oversight. Diversification, exports, and product pivots give it wide room to maneuver.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__individual_right_reading, firearms_industry, beneficiary,
    powerful, biographical, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(second_amendment_arms_right__individual_right_reading, firearms_industry, payer).

% Membership and litigation organizations whose mission, fundraising, and member identity center on defending the guarantee. They fund and argue the test cases that define the doctrine's reach. Their institutional existence is inseparable from the arrangement; winding down would dissolve their reason for being.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__individual_right_reading, gun_rights_advocacy_organizations, beneficiary,
    organized, generational, identity_locked, national).

% Federal and state agencies and legislatures that write and administer firearms regulation. Each doctrinal expansion narrows the set of measures they may enforce: licensing schemes, bans, and carry restrictions face heightened scrutiny or invalidation. They cannot abandon their public-safety mandate and must operate inside whatever space the doctrine leaves, absorbing litigation losses and repeated redesign costs.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__individual_right_reading, firearms_regulatory_authorities, payer,
    institutional, generational, trapped, national).

% Neighborhoods, disproportionately urban and low-income, where firearm homicide concentrates. They bear the public-safety consequences of broad access and narrowed regulation: shooting cycles, policing burdens, and trauma economies. Moving away is possible in principle but costly and disruptive, and the exposure reproduces across generations in place. Community organizations build coalitions and run local intervention programs, but their leverage over the constitutional frame is thin.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__individual_right_reading, gun_violence_exposed_communities, payer,
    moderate, biographical, trapped, regional).

% Defines the reading's content case by case: Heller recognized individual defense of the home, McDonald extended the guarantee against the states, Bruen installed text-history-tradition review, Rahimi began marking limits. It administers the arrangement and could reshape or reverse it, as it reversed the prior militia-centered doctrine in 2008. Its interpretive choices are the arrangement's operating mechanism.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__individual_right_reading, supreme_court, agenda_setter,
    institutional, generational, mobile, national).

% Organizations and movements seeking stronger regulation. They litigate, legislate, and mobilize after mass shootings, but the text-history-tradition method admits founding-era sources and excludes the contemporary epidemiological and sociological record they produce, so their expertise is structurally inadmissible in the forum that decides outcomes. Their policy goals are precisely the ones the arrangement forecloses.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__individual_right_reading, gun_control_advocacy_movements, excluded,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(second_amendment_arms_right__individual_right_reading, gun_control_advocacy_movements, payer).

% Scholars who map the doctrine's coherence, audit the historical method, and document gaps between founding evidence and modern application. They take no side in enforcement; their output informs courts, litigants, and reform proposals.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__individual_right_reading, constitutional_law_academy, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(second_amendment_arms_right__individual_right_reading, firearms_industry).
narrative_ontology:fixing_cost_class(second_amendment_arms_right__individual_right_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Settles, ex ante and by entrenched rule, the boundary between private armament and government power: citizens hold an enforceable assurance against federal disarmament, gun owners in restrictive jurisdictions receive judicial protection from majorities, and the government-force question is moved from recurring political struggle into adjudication.
% TRANSFER_FUNCTION: Moves regulatory discretion from federal and state governments to individual gun owners; moves decisions about arms possession from legislatures to courts; and distributes the residual public-safety risk of broad access onto the communities where violence concentrates, while commercial demand flows to manufacturers.
% ABSENT_VOICES: Violence-exposed residents and public-health researchers have no seat: the governing interpretive method admits founding-era sources and excludes contemporary empirical evidence, so those who bear the arrangement's diffuse costs cannot testify in the forum that decides its reach. The organized militia the preamble names is likewise absent; the institution the text foregrounds plays no role in the doctrine's operation.
% DISAPPEARANCE_RATIONALE: Overnight repeal or abandonment would hand federal and state governments immediate authority to prohibit, license, or confiscate; tens of millions of lawfully held arms would become regulable at once; the industry's domestic market would contract sharply; and a massive political-legal conflict over implementation would erupt. Nearly every arrangement in American firearms governance depends on the guarantee.
% FOUNDING_PROBLEM: Fear that a distant central government with a standing army could disarm the people and destroy the state militias; the arrangement embedded the militia-era answer that the people's arms stand beyond federal reach.
% FOUNDING_PROBLEM_CORROBORATION: Professional historiography outside the beneficiary set (the standard account of the founding-era militia and standing-army debates) attests the original problem. Whether it remains live is disputed between gun-rights scholarship and the historical mainstream, which holds that the militia system the problem presupposed has been superseded; no neutral body certifies it live.
narrative_ontology:disappearance_verdict(second_amendment_arms_right__individual_right_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_arms_right__individual_right_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_arms_right__individual_right_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(second_amendment_arms_right__individual_right_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_arms_right__individual_right_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_arms_right__individual_right_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(second_amendment_arms_right__individual_right_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(second_amendment_arms_right__individual_right_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction 0.58 is reading-indexed over the standing arrangement (the Heller-through-Bruen regime), not over any endorsed alternative: the individual-right reading honestly concedes the arrangement channels real costs, stripped regulatory capacity by design, diffuse safety externalities, aggressive post-Bruen enforcement sweeps, while maintaining that the protection itself takes nothing. Suppression 0.55 measures foreclosure of regulatory alternatives, not coercion of persons; it is authored as a raw structural property and left unscaled. Theater 0.45: the militia preamble does little operative work and the history-heavy method generates law-office-history pageantry, but adjudication is real. Accessibility collapse 0.50: accepting the frame collapses confiscation and licensing-heavy designs while leaving shall-issue permitting, prohibited-categories rules, and sensitive-places rules viable. Resistance 0.65: a permanent, well-funded opposition contests every expansion. The three tracked series share one eight-point grid (2008-2026); the 2026 row is projected. The rise tracks doctrine: Heller (individual right recognized), McDonald (incorporation), Bruen (new method and scope), with Rahimi as a partial brake. A cyclical pattern exists in the regulatory-attempt layer (mass shooting, reform surge, litigation, relaxation) but the constraint-layer series is monotonic, so no cyclic grid was authored.
 *
 * PERSPECTIVAL GAP:
 *   The owner and advocacy seats experience a shield: a pre-existing liberty government may not touch. The regulator seat experiences a steadily shrinking mandate punctuated by litigation losses. The community seat experiences imposed risk without a voice in the forum that sets the rule. The Court experiences neutral arbitration of text and history. One structure, four incompatible phenomenologies; the engine computes this per-seat divergence from power, exit, and role rather than accepting any seat's self-description.
 *
 * DIRECTIONALITY LOGIC:
 *   Owners and advocacy organizations sit near the beneficiary pole (d roughly 0.05-0.15): the guarantee subsidizes them, and identity lock deepens their stake. The industry sits low but not lowest (d roughly 0.30): it collects the commercial upside yet also answers to residual federal oversight, hence its dual beneficiary/payer declaration. Regulators sit near the target pole (d roughly 0.85-0.95): the arrangement's entire operation consists of removing their instruments. Exposed communities sit high (d roughly 0.70-0.80): they bear diffuse costs they did not consent to and cannot exit cheaply. The Court sits near symmetric (d roughly 0.4-0.5): it grants and withdraws doctrine, collecting institutional authority either way. No directionality overrides were needed: the beneficiary/victim declarations plus exit differentiation already separate the seats, and an override keyed to a shared power atom (three seats sit at 'organized', two at 'institutional') would smear distinct positions onto the wrong agents.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem, militia security against federal disarmament, is contested-obsolescent: the organized militia it presupposed no longer anchors the doctrine's operation, and the preamble's language survives largely as rhetoric while the operative right protects individual carrying. Yet the arrangement is not a piton: its coordination function has transformed rather than atrophied (self-defense assurance, minority protection against majoritarian confiscation, judicial settlement of the force question are live functions), and its enforcement is vigorous rather than theatrical maintenance. Classifying it tangled_rope holds both truths at once, a real coordination core and real asymmetric costs, and thereby avoids the twin errors: the pure-rope reading ignores who pays; the pure-snare reading denies the coordination and mistakes a constitutional settlement for a protection racket. The R5 mismatch check finds status=contested paired with verdict=world_rearranges, so no dead-mandate zombie flag is warranted.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_location,
    'This constraint is one reading of kernel second_amendment_arms_right (reading: individual_right_reading). What would change structurally if a sibling reading governed instead?',
    'Not resolvable within any single framework; tracked via the sibling stories. Adoption of the collective_right_reading would remove individual owners from the beneficiary set entirely, install state militias as the protected party, and flip regulatory authority from constrained party to administrator; adoption of the civic_republican_reading would re-center militia service obligations and condition the liberty on civic function.',
    'The classification of THIS constraint (tangled_rope with owners as beneficiaries and regulators as targets) would invert under the collective reading; epsilon would be re-authored over a different beneficiary/victim structure. The disagreement is located in the right-holder and the preexistence premise, not in the metrics.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_location, conceptual, 'Committer structure: which kernel, which reading, what siblings would change.').

omega_variable(
    preexistence_naturalness,
    'Is the right genuinely pre-existing (a natural liberty the text merely acknowledges, as this reading''s reference frame asserts) or constituted by the constitutional settlement itself?',
    'Jurisprudential and comparative analysis: if the right''s content varies with enactment, amendment, and doctrinal choice across polities with no convergence on a natural baseline, the constituted reading wins; if cross-jurisdictional practice converges on a stable personal-armament liberty, the preexistence claim strengthens.',
    'If constituted, the constraint is ordinary political construction with no mountain-like inviolability, and its persistence depends wholly on continued enforcement; if pre-existing, the arrangement approaches a discovered boundary whose violation is error rather than policy choice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(preexistence_naturalness, conceptual, 'Whether the reading''s naturality premise describes discovery or construction.').

omega_variable(
    externality_attribution,
    'Are the public-safety costs borne by violence-exposed communities attributable to this arrangement''s operation, or to criminal behavior that would persist under any regulatory regime?',
    'Quasi-experimental study of Bruen-driven policy changes (carry expansion, permitless-carry adoptions) on violence rates, with jurisdiction-matched controls.',
    'If costs are not attributable, communities drop out of the victim set and the structure moves toward rope; if attributable, the tangled_rope classification firms and the community seat''s directionality rises toward full target.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(externality_attribution, empirical, 'Causal attribution of the community cost externality.').

omega_variable(
    bruen_trajectory_durability,
    'Will the post-2022 expansionary enforcement persist, accelerate, or self-limit through Rahimi-style carve-outs, circuit splits, or Court retrenchment?',
    'Track lower-court invalidation rates, Supreme Court grant patterns, and doctrinal qualification cases over 2026-2032.',
    'Determines whether the rising extractiveness series continues toward snare-adjacent territory or plateaus as a stable tangled_rope; a durable expansion would also raise the regulator seat''s effective extraction further.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(bruen_trajectory_durability, empirical, 'Durability of the current enforcement trajectory.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_arms_right__individual_right_reading, 2008, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t2008, second_amendment_arms_right__individual_right_reading, theater_ratio, 2008, 0.3).
narrative_ontology:measurement_basis(seco_tr_t2008, observed).
narrative_ontology:measurement(seco_tr_t2010, second_amendment_arms_right__individual_right_reading, theater_ratio, 2010, 0.31).
narrative_ontology:measurement_basis(seco_tr_t2010, observed).
narrative_ontology:measurement(seco_tr_t2013, second_amendment_arms_right__individual_right_reading, theater_ratio, 2013, 0.33).
narrative_ontology:measurement_basis(seco_tr_t2013, observed).
narrative_ontology:measurement(seco_tr_t2016, second_amendment_arms_right__individual_right_reading, theater_ratio, 2016, 0.34).
narrative_ontology:measurement_basis(seco_tr_t2016, observed).
narrative_ontology:measurement(seco_tr_t2019, second_amendment_arms_right__individual_right_reading, theater_ratio, 2019, 0.35).
narrative_ontology:measurement_basis(seco_tr_t2019, observed).
narrative_ontology:measurement(seco_tr_t2022, second_amendment_arms_right__individual_right_reading, theater_ratio, 2022, 0.4).
narrative_ontology:measurement_basis(seco_tr_t2022, observed).
narrative_ontology:measurement(seco_tr_t2024, second_amendment_arms_right__individual_right_reading, theater_ratio, 2024, 0.43).
narrative_ontology:measurement_basis(seco_tr_t2024, observed).
narrative_ontology:measurement(seco_tr_t2026, second_amendment_arms_right__individual_right_reading, theater_ratio, 2026, 0.45).
narrative_ontology:measurement_basis(seco_tr_t2026, projected).

% Extraction over time
narrative_ontology:measurement(seco_be_t2008, second_amendment_arms_right__individual_right_reading, base_extractiveness, 2008, 0.38).
narrative_ontology:measurement_basis(seco_be_t2008, observed).
narrative_ontology:measurement(seco_be_t2010, second_amendment_arms_right__individual_right_reading, base_extractiveness, 2010, 0.42).
narrative_ontology:measurement_basis(seco_be_t2010, observed).
narrative_ontology:measurement(seco_be_t2013, second_amendment_arms_right__individual_right_reading, base_extractiveness, 2013, 0.44).
narrative_ontology:measurement_basis(seco_be_t2013, observed).
narrative_ontology:measurement(seco_be_t2016, second_amendment_arms_right__individual_right_reading, base_extractiveness, 2016, 0.46).
narrative_ontology:measurement_basis(seco_be_t2016, observed).
narrative_ontology:measurement(seco_be_t2019, second_amendment_arms_right__individual_right_reading, base_extractiveness, 2019, 0.49).
narrative_ontology:measurement_basis(seco_be_t2019, observed).
narrative_ontology:measurement(seco_be_t2022, second_amendment_arms_right__individual_right_reading, base_extractiveness, 2022, 0.54).
narrative_ontology:measurement_basis(seco_be_t2022, observed).
narrative_ontology:measurement(seco_be_t2024, second_amendment_arms_right__individual_right_reading, base_extractiveness, 2024, 0.56).
narrative_ontology:measurement_basis(seco_be_t2024, observed).
narrative_ontology:measurement(seco_be_t2026, second_amendment_arms_right__individual_right_reading, base_extractiveness, 2026, 0.58).
narrative_ontology:measurement_basis(seco_be_t2026, projected).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t2008, second_amendment_arms_right__individual_right_reading, suppression_requirement, 2008, 0.35).
narrative_ontology:measurement_basis(seco_su_t2008, observed).
narrative_ontology:measurement(seco_su_t2010, second_amendment_arms_right__individual_right_reading, suppression_requirement, 2010, 0.38).
narrative_ontology:measurement_basis(seco_su_t2010, observed).
narrative_ontology:measurement(seco_su_t2013, second_amendment_arms_right__individual_right_reading, suppression_requirement, 2013, 0.4).
narrative_ontology:measurement_basis(seco_su_t2013, observed).
narrative_ontology:measurement(seco_su_t2016, second_amendment_arms_right__individual_right_reading, suppression_requirement, 2016, 0.42).
narrative_ontology:measurement_basis(seco_su_t2016, observed).
narrative_ontology:measurement(seco_su_t2019, second_amendment_arms_right__individual_right_reading, suppression_requirement, 2019, 0.44).
narrative_ontology:measurement_basis(seco_su_t2019, observed).
narrative_ontology:measurement(seco_su_t2022, second_amendment_arms_right__individual_right_reading, suppression_requirement, 2022, 0.52).
narrative_ontology:measurement_basis(seco_su_t2022, observed).
narrative_ontology:measurement(seco_su_t2024, second_amendment_arms_right__individual_right_reading, suppression_requirement, 2024, 0.54).
narrative_ontology:measurement_basis(seco_su_t2024, observed).
narrative_ontology:measurement(seco_su_t2026, second_amendment_arms_right__individual_right_reading, suppression_requirement, 2026, 0.55).
narrative_ontology:measurement_basis(seco_su_t2026, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_arms_right__individual_right_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(second_amendment_arms_right__individual_right_reading, second_amendment_arms_right__collective_right_reading).
narrative_ontology:affects_constraint(second_amendment_arms_right__individual_right_reading, second_amendment_arms_right__civic_republican_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'the Second Amendment right' decomposes into three structurally distinct constraints sharing one kernel text. This file is the individual_right_reading; the collective_right_reading and civic_republican_reading are separate stories with their own epsilon, beneficiary/victim structures, and classifications. The upstream/downstream relation runs through interpretive dominance: whichever reading controls doctrine determines which parties appear in the other stories' structural data. All three files link one another via network.affects_constraints; no single story hedges epsilon across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
