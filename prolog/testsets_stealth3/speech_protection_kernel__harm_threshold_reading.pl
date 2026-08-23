% ============================================================================
% CONSTRAINT STORY: speech_protection_kernel__harm_threshold_reading
% ============================================================================
% Version: 7.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_speech_protection_kernel__harm_threshold_reading, []).

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
 *   constraint_id: speech_protection_kernel__harm_threshold_reading
 *   human_readable: Speech Protection Conditioned on Demonstrable Victim Harm (Harm-Threshold Reading)
 *   domain: constitutional_law/political_philosophy/communication_rights
 *
 * SUMMARY:
 *   In contemporary constitutional orders, speech protection is not
 *   categorical: it holds only where no demonstrable harm to identifiable
 *   victims can be shown, and courts, legislatures, and platform moderators
 *   administer that condition. Defamation redress, harassment injunctions,
 *   incitement prohibitions, and platform harm policies are the working
 *   surface of the arrangement. Its operation runs on two legs at once: a
 *   genuine protective function that delivers real remedies to injured
 *   parties, and an asymmetric payment structure in which speakers nearest
 *   the boundary — especially minority dissenters — carry chilling effects,
 *   defense costs, and enforcement risk that well-resourced claimants and
 *   administering institutions do not. CONSTRAINT FAMILY NOTE: this file
 *   instantiates the harm-threshold reading of speech_protection_kernel; the
 *   absolutist, marketplace, dignity, and democratic-participation readings
 *   are separate constraint files linked through the network edges. Epsilon
 *   diverges across the family because each reading fixes a different
 *   restriction trigger and a different victim set: the absolutist reading
 *   protects near-categorically and locates its costs on the targets of
 *   speech rather than speakers; the marketplace reading tolerates falsehood
 *   as fuel for correction; the dignity reading keys restriction to
 *   subordination function rather than demonstrated individual injury,
 *   pulling group-directed expression into the unprotected set; the
 *   democratic-participation reading concentrates protection on political
 *   expression. This file authors epsilon for the standing
 *   conditional-protection arrangement as this reading assesses it —
 *   moderate, because the remedies are real while the payments fall unevenly.
 *   KEY AGENTS (by structural relationship): - constitutional_courts:
 *   agenda-setter (institutional/constrained) — decides demonstrability,
 *   accumulates jurisdiction with each adjudication -
 *   content_platform_operators: agenda-setter with a beneficiary
 *   side-position (institutional/arbitrage) — enforces the threshold at
 *   scale, collects liability insulation while paying compliance costs -
 *   victims_of_demonstrable_harm: beneficiary (powerless/trapped) — collect
 *   redress when injury is provable; many provable-in-principle harms go
 *   unproven - majority_offense_coalitions: beneficiary (organized/mobile) —
 *   convert sensibility into harm claims and collect the quieting of critics
 *   - boundary_case_speakers: payer (moderate/constrained) — bear
 *   anticipatory softening, defense costs, outcome uncertainty -
 *   minority_dissent_speakers: payer (powerless/identity_locked) — attract
 *   harm claims most readily; exit means abandoning the self their voice
 *   constitutes - audience_information_seekers: excluded
 *   (moderate/constrained) — lose informational access with no procedural
 *   seat - comparative_free_expression_scholars: observer
 *   (analytical/analytical) — see the full structure from outside any single
 *   order
 *
 * KEY AGENTS:
 *   - constitutional_courts: agenda-setter (institutional/constrained) — interprets the harm condition, decides what counts as demonstrable, and accrues precedent and interpretive jurisdiction with every adjudication
 *   - content_platform_operators: agenda-setter with beneficiary side-position (institutional/arbitrage) — translates the threshold into moderation policy, collects liability insulation and arbitrariness-cover while absorbing compliance burdens
 *   - victims_of_demonstrable_harm: primary beneficiary (powerless/trapped) — obtain investigation, removal, damages, or orders when injury is demonstrated; demonstration itself filters out the resource-poor
 *   - majority_offense_coalitions: secondary beneficiary (organized/mobile) — press community-harm claims against offensive expression and collect critic-quieting where adjudicators accept the framing
 *   - boundary_case_speakers: primary payer (moderate/constrained) — expression near the line; pay in self-censorship, legal fees, and outcome uncertainty
 *   - minority_dissent_speakers: primary payer (powerless/identity_locked) — dissent that challenges dominant interests draws harm claims most readily; silence would dissolve the identity their speech carries
 *   - audience_information_seekers: excluded seat (moderate/constrained) — bear curtailed access with zero procedural representation
 *   - comparative_free_expression_scholars: analytical observer (analytical/analytical) — code restriction triggers and category expansion across regimes without standing inside any
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_protection_kernel__harm_threshold_reading, 0.6).
domain_priors:suppression_score(speech_protection_kernel__harm_threshold_reading, 0.6).
domain_priors:theater_ratio(speech_protection_kernel__harm_threshold_reading, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_protection_kernel__harm_threshold_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(speech_protection_kernel__harm_threshold_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(speech_protection_kernel__harm_threshold_reading, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_protection_kernel__harm_threshold_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(speech_protection_kernel__harm_threshold_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_protection_kernel__harm_threshold_reading, tangled_rope).
narrative_ontology:human_readable(speech_protection_kernel__harm_threshold_reading, "Speech Protection Conditioned on Demonstrable Victim Harm (Harm-Threshold Reading)").
narrative_ontology:topic_domain(speech_protection_kernel__harm_threshold_reading, "constitutional_law/political_philosophy/communication_rights").

domain_priors:requires_active_enforcement(speech_protection_kernel__harm_threshold_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_protection_kernel__harm_threshold_reading, '397acfbe-9091-4db8-89bc-8c64b92d8943').
narrative_ontology:cs_kernel_codification('397acfbe-9091-4db8-89bc-8c64b92d8943', fixed_text).
narrative_ontology:cs_authority_grounding('397acfbe-9091-4db8-89bc-8c64b92d8943', lineage).
narrative_ontology:cs_interpretation_layer_present('397acfbe-9091-4db8-89bc-8c64b92d8943').
narrative_ontology:cs_reading_relation('397acfbe-9091-4db8-89bc-8c64b92d8943', speech_protection_kernel__absolutist_reading, forecloses).
narrative_ontology:cs_reading_relation('397acfbe-9091-4db8-89bc-8c64b92d8943', speech_protection_kernel__marketplace_reading, coexists_with).
narrative_ontology:cs_reading_relation('397acfbe-9091-4db8-89bc-8c64b92d8943', speech_protection_kernel__dignity_reading, coexists_with).
narrative_ontology:cs_reading_relation('397acfbe-9091-4db8-89bc-8c64b92d8943', speech_protection_kernel__democratic_participation_reading, coexists_with).
narrative_ontology:cs_axiom('397acfbe-9091-4db8-89bc-8c64b92d8943', foundational, victim_harm_overrides_speaker_autonomy).
narrative_ontology:cs_axiom_status(victim_harm_overrides_speaker_autonomy, holdable).
narrative_ontology:cs_axiom_grounding('397acfbe-9091-4db8-89bc-8c64b92d8943', victim_harm_overrides_speaker_autonomy, deontological).
narrative_ontology:cs_axiom('397acfbe-9091-4db8-89bc-8c64b92d8943', secondary, restriction_requires_demonstrated_identifiable_harm).
narrative_ontology:cs_axiom_status(restriction_requires_demonstrated_identifiable_harm, holdable).
narrative_ontology:cs_axiom_grounding('397acfbe-9091-4db8-89bc-8c64b92d8943', restriction_requires_demonstrated_identifiable_harm, empirically_contingent).
narrative_ontology:cs_reference_frame('397acfbe-9091-4db8-89bc-8c64b92d8943', millian_default_liberty_harm_limit).
narrative_ontology:cs_drift_state('397acfbe-9091-4db8-89bc-8c64b92d8943', contemporary_category_expansion_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('397acfbe-9091-4db8-89bc-8c64b92d8943', '').
narrative_ontology:cs_kernel_id(speech_protection_kernel__harm_threshold_reading, speech_protection_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_protection_kernel__harm_threshold_reading, victims_of_demonstrable_harm).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__harm_threshold_reading, majority_offense_coalitions).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__harm_threshold_reading, content_platform_operators).
narrative_ontology:constraint_victim(speech_protection_kernel__harm_threshold_reading, boundary_case_speakers).
narrative_ontology:constraint_victim(speech_protection_kernel__harm_threshold_reading, minority_dissent_speakers).
narrative_ontology:constraint_vindicates(speech_protection_kernel__harm_threshold_reading, millian_harm_principle).
narrative_ontology:constraint_vindicates(speech_protection_kernel__harm_threshold_reading, proportional_restriction_balancing).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interpret and apply the harm condition in every speech dispute that reaches them: decide whether asserted injury is demonstrable, which harm categories count, and what evidence suffices. Each adjudication extends their interpretive authority over public expression and generates precedent that narrows or widens the protected zone. Their position is doctrinally bound — precedent, statute, and treaty obligation hold them in place; they cannot decline to adjudicate.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__harm_threshold_reading, constitutional_courts, agenda_setter,
    institutional, generational, constrained, national).

% Run large-scale speech venues and translate legal harm thresholds into content moderation policy enforced on billions of posts. The harm framework hands them a principled warrant that shields moderation decisions from charges of arbitrariness and lowers liability exposure; in exchange they absorb compliance costs, review volume, and regulatory penalties when their enforcement trails legal expectations. Incorporation choices, market withdrawals, and lobbying for lighter regimes keep jurisdictional arbitrage open to them.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__harm_threshold_reading, content_platform_operators, agenda_setter,
    institutional, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(speech_protection_kernel__harm_threshold_reading, content_platform_operators, beneficiary).

% People who sustain concrete injury from others' expression — destroyed reputations, targeted harassment, incitement that turns toward them — and who obtain investigation, removal, damages, or protective orders when they can demonstrate the injury to an adjudicator. Demonstration takes evidence, time, and usually legal help many do not have, so a large share of qualifying injuries never converts into remedy. They cannot step outside the speech environment that targets them.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__harm_threshold_reading, victims_of_demonstrable_harm, beneficiary,
    powerless, biographical, trapped, national).

% Organized constituencies that press claims that offensive, blasphemous, or critical expression injures the community. Where adjudicators accept sensibility-based claims as demonstrable harm, these coalitions collect the quieting of critics and rivals; where adjudicators reject such claims they forfeit nothing durable. Participation is episodic and mobile — they redeploy to whichever forum currently accepts their framing.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__harm_threshold_reading, majority_offense_coalitions, beneficiary,
    organized, biographical, mobile, national).

% Speakers whose expression sits near the threshold: pointed satire, harsh criticism of institutions, scientific dissent, confrontational art. They carry the anticipatory costs — softened language, dropped subjects, attorney fees for borderline publications, and the unpredictability of evidentiary standards that shift beneath them. Leaving means falling silent; publishing abroad reaches thinner audiences and invites foreign-enforcement friction.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__harm_threshold_reading, boundary_case_speakers, payer,
    moderate, biographical, constrained, national).

% Dissidents whose speech challenges dominant groups' interests and therefore attracts harm claims most readily — their opponents are well resourced and their expression is least familiar to adjudicators. Silence is not a workable exit: their public voice constitutes their cause and their sense of who they are, so remaining in the fight is bound up with identity even when the personal toll is heavy. Coalition with similarly situated speakers is possible but organizationally fragile under repeated enforcement contact.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__harm_threshold_reading, minority_dissent_speakers, payer,
    powerless, generational, identity_locked, national).

% Listeners and readers whose access to contested information narrows whenever expression is restrained upstream. No adjudication solicits their testimony: they appear in the process neither as claimants nor as respondents, although the informational environment they live in is precisely what the arrangement governs. Their workarounds — archives, mirrors, foreign outlets — are incomplete substitutes.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__harm_threshold_reading, audience_information_seekers, excluded,
    moderate, biographical, constrained, national).

% Researchers who compare harm-conditioned speech regimes across jurisdictions: coding what triggers restriction, tracking category expansion, and auditing whose claims succeed. They see the whole structure from outside any single legal order and periodically feed findings back into judicial reform debates and treaty-body reviews.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__harm_threshold_reading, comparative_free_expression_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(speech_protection_kernel__harm_threshold_reading, constitutional_courts).
narrative_ontology:fixing_cost_class(speech_protection_kernel__harm_threshold_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a publicly administrable criterion for when expression may legitimately be limited: identifiable victims who can demonstrate concrete injury obtain redress and restraint of the harming expression, so recurrent speech conflicts resolve through adjudication rather than private retaliation, crowd sanction, or unbounded official whim.
% TRANSFER_FUNCTION: Moves expressive freedom from speakers whose expression crosses, or risks being judged to cross, the harm boundary toward claimants' protective interests and adjudicators' discretionary authority; moves decision power over public discourse from speakers and audiences to courts, legislatures, and platform enforcement staff; moves legal and reputational risk onto speakers nearest the line.
% ABSENT_VOICES: Audiences: the people whose informational access shrinks when expression is restricted hold no procedural seat — adjudication recognizes only claiming victims and accused speakers. Second, speakers of not-yet-defined categories: whoever will express what tomorrow's expanded harm definitions capture cannot object before the definitions exist. Both stand outside the room where the boundary gets drawn.
% DISAPPEARANCE_RATIONALE: If the harm condition vanished overnight, defamation, harassment, and incitement remedies lapse and targets of those harms lose all protection; platforms lose the warrant for moderation and either abandon enforcement or improvise private standards; every constitutional order must immediately re-found speech law as either categorical protection or unconstrained official discretion — the surrounding architecture rearranges around whichever successor wins.
% FOUNDING_PROBLEM: Liberal constitutionalism inherited an unstable pair of failures: categorical speech protection left defamation victims, harassment targets, and communities facing incitement without remedy, pushing them toward private vengeance; while handing officials unrestricted silencing power invited persecution of dissent. Mill's harm principle proposed the escape route — protect expression as the default, allow restriction only where demonstrable harm to identifiable others can be shown.
% FOUNDING_PROBLEM_CORROBORATION: Attested from outside the benefiting parties: mid-twentieth-century constitutional settlements across mutually hostile legal traditions — post-war German Basic Law, occupied Japan, the ICCPR drafting record — independently converged on harm-conditioned protection, and civil-liberties organizations that campaign against threshold expansion nevertheless concede in litigation positions that its narrow core answers real injuries. Convergence among parties who did not share the arrangement's beneficiary structure, recorded in ratification histories and case files, corroborates the genealogy.
narrative_ontology:disappearance_verdict(speech_protection_kernel__harm_threshold_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_protection_kernel__harm_threshold_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_protection_kernel__harm_threshold_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(speech_protection_kernel__harm_threshold_reading, 'none', 1).
narrative_ontology:epsilon_provenance(speech_protection_kernel__harm_threshold_reading, 0.6, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(speech_protection_kernel__harm_threshold_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(speech_protection_kernel__harm_threshold_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(speech_protection_kernel__harm_threshold_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.60: the standing arrangement delivers genuine remedies (which pulls the measure down from snare territory) while withdrawing expressive freedom asymmetrically from boundary and minority speakers, whose payments are structural — anticipatory self-censorship, defense costs, enforcement exposure — rather than incidental. Suppression 0.60 is authored as a RAW structural property: the penal statutes, takedown powers, prosecution exposure, and platform enforcement that activate when the threshold is met; per the framework, only extractiveness is scaled by directionality and scope in the engine's computation, and this commentary reflects that division. Theater_ratio 0.31: impact assessments, expert hearings, and consultation exercises around harm determination are partly evidentiary and partly legitimating performance that dresses predetermined outcomes as findings. Accessibility_collapse 0.45: exits exist but are partial — foreign publication, anonymity, jurisdictional arbitrage for platforms — because expression is territorial and audience-dependent. Resistance 0.55: organized civil-liberties litigation, press-freedom campaigns, and scholarly audit constitute sustained, effective pushback against category expansion. The three temporal series run on ONE shared grid (1965, 1978, 1991, 2003, 2015, 2026) with every metric authored at every point — no per-metric grids, no scalar substitution at gaps. All three trajectories are monotonic ratchets, not cycles: category expansion, enforcement-infrastructure maturation (specialized units, platform duties of care), and rising legitimation ritual proceed together without an observable relaxation phase, so no cyclical-reinforcement reading is warranted here. End-state series values equal the base_properties scalars by construction.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from identical structure. Payer seats (boundary_case_speakers, minority_dissent_speakers) experience the arrangement as unpredictable restriction risk priced onto their expression; the beneficiary seats (victims_of_demonstrable_harm, majority_offense_coalitions) experience the same structure as protection and remedy delivery; the agenda-setting court seat experiences it as a balancing mandate it administers faithfully; the excluded audience seat experiences it as a curated information environment it never consented to and cannot contest procedurally. Nothing in the arrangement contradicts itself — the divergence is positional, produced by who pays and who collects under the same rule. Coalition potential matters for the weakest seats: minority_dissent_speakers are individually powerless but their claims gain traction when aggregated with boundary_case_speakers and civil-liberties litigators; the arrangement's enforcement pattern (case-by-case, identity-specific) works against exactly that aggregation.
 *
 * DIRECTIONALITY LOGIC:
 *   Declarations map to structure as follows. victims_of_demonstrable_harm sit near the beneficiary pole: remedies and restraint flow toward them when they can prove injury, and they pay nothing back except the effort of proof — d near 0.05-0.15. majority_offense_coalitions collect critic-quieting where their claims land and lose nothing durable where they fail — low d with high mobility. content_platform_operators are genuinely dual-positioned: they collect liability insulation and arbitrariness-cover (benefit side) but pay compliance and penalty costs (cost side), netting modestly positive — slightly above the pure-beneficiary derivation. boundary_case_speakers sit high on the target side: their payment is the product, their exit is silence, d near 0.8-0.85. minority_dissent_speakers sit at the extreme: they attract the claims, lack the resources to contest them, and cannot exit without abandoning the cause their voice expresses — identity-lock pushes them toward full-target, d near 1.0. The identity-lock mechanism here is ideological identity fusion: the dissenter's public voice constitutes both the movement and the self; were that frame to break (cause achieved, disowned, or transferred to successors), d would drop sharply and the seat's computed classification would soften with it. constitutional_courts derive low d as the administering beneficiary of interpretive authority.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against two symmetrical mislabelings. Read as pure protection (rope framing, the reading's self-description at its most charitable), the arrangement's asymmetric payments vanish: the chill concentrated on minority dissenters, the majority weaponization channel, and the adjudicative-authority accretion all disappear from the ledger. Read as pure censorship (snare framing, the civil-libertarian caricature), the real remedies delivered to injured parties disappear and every defamation award looks like repression. The tangled_rope claim holds both halves in view simultaneously: genuine coordination (conflict resolution without private retaliation) AND asymmetric extraction through the same structure. On obsolescence: the founding problem is live — defamation, harassment, and incitement still injure — so no mandatrophy is declared and none is resolved. The live risk is directional drift: the measurement series show monotonic growth in extractiveness and enforcement requirement across sixty years, driven by category expansion beyond the demonstrable-and-individually-identified harm the founding formulation specified. If the weaponization omega resolves toward dominant-coalition predominance, the coordination leg atrophies while the structure persists, and this arrangement migrates toward the extraction-dominated pole with the theater ratio marking the transition. The temporal record exists precisely to catch that migration early.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest_location,
    'Within the speech-protection kernel, which structural element separates this harm-threshold reading from its siblings — the restriction trigger, the recognized victim set, or the evidentiary bar for demonstration — and which divergence drives the family''s classification differences?',
    'Comparative doctrinal mapping across the five reading files: tabulate restriction triggers, recognized victim sets, and demonstration standards side by side, and locate where per-seat classifications diverge.',
    'If the trigger element drives the divergence, harmonizing triggers could merge readings into one constraint; if the victim set drives, the readings remain irreducibly distinct constraints with distinct epsilon values and must stay separate files.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest_location, conceptual, 'Locates the load-bearing structural divergence among sibling readings of the speech-protection kernel.').

omega_variable(
    harm_demonstration_standard,
    'What evidentiary threshold makes a harm count as demonstrable, and does the operative standard vary across harm types — physical, economic, psychological, dignitary?',
    'Audit adjudicated restriction decisions across jurisdictions, correlating classes of accepted evidence with grant rates, stratified by harm category.',
    'A loose standard converts precaution into routine restriction and raises effective extraction on speaker seats; a strict standard leaves tangible injuries unredressed and hollows out the arrangement''s coordination half.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(harm_demonstration_standard, conceptual, 'Contestability of the demonstrability bar that defines the entire reading.').

omega_variable(
    chill_incidence_distribution,
    'Does the chilling effect of harm-conditioned enforcement fall evenly across the speaker population, or concentrate on boundary-case and minority dissent speakers?',
    'Cross-tabulate self-censorship survey data and restriction case outcomes by speaker resource level and distance from mainstream positions.',
    'Concentrated incidence confirms the asymmetric payment structure and pushes computed per-seat classifications of the speaker seats toward the full-target end; proportional incidence supports the balanced-coordination framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(chill_incidence_distribution, empirical, 'Incidence of suppression and chill costs across speaker strata.').

omega_variable(
    harm_claim_weaponization_direction,
    'Do successful harm claims originate predominantly from structurally vulnerable victims seeking protection, or from dominant coalitions converting sensibility into harm?',
    'Code a corpus of granted restrictions by claimant power position and target speech content; compare against claimant population base rates to detect overrepresentation.',
    'Predominant dominant-coalition usage indicates the threshold operating as a majoritarian suppression instrument riding on protective cover, shifting the story toward the extraction-dominated pole of its type.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(harm_claim_weaponization_direction, empirical, 'Direction of harm-claim utilization across claimant power positions.').

omega_variable(
    chill_structural_vs_internalized,
    'Is measured suppression carried by active enforcement machinery alone, or substantially sustained by anticipatory self-censorship that would persist even if enforcement receded?',
    'Compare speech volumes and topic diversity before and after enforcement-retrenchment or decriminalization episodes within comparable jurisdictions.',
    'An internalized share means suppression outlives enforcement decay, so the constraint''s effective persistence exceeds what enforcement statistics show and any enforcement-softening trend in the temporal series overstates liberalization.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(chill_structural_vs_internalized, empirical, 'Structural versus internalized composition of the measured suppression.').

omega_variable(
    authority_grounding_framing,
    'Is the threshold''s adjudicative authority grounded in constitutional lineage — founding texts plus authorized transmission — or in the accumulated case-law corpus functioning as a practice-formed kernel?',
    'Trace justification chains in landmark judgments: count reliance on textual hooks versus citation networks to prior adjudications.',
    'A practice-grounding verdict reframes kernel codification as distributed rather than fixed_text and relocates drift detection from text-amendment watching to citation-network monitoring; classification consequences follow the recomputed commitment-system profile.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authority_grounding_framing, conceptual, 'Under-determination between lineage and practice framings of the adjudicative authority.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_protection_kernel__harm_threshold_reading, 1965, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spee_tr_t1965, speech_protection_kernel__harm_threshold_reading, theater_ratio, 1965, 0.15).
narrative_ontology:measurement_basis(spee_tr_t1965, observed).
narrative_ontology:measurement(spee_tr_t1978, speech_protection_kernel__harm_threshold_reading, theater_ratio, 1978, 0.18).
narrative_ontology:measurement_basis(spee_tr_t1978, observed).
narrative_ontology:measurement(spee_tr_t1991, speech_protection_kernel__harm_threshold_reading, theater_ratio, 1991, 0.21).
narrative_ontology:measurement_basis(spee_tr_t1991, observed).
narrative_ontology:measurement(spee_tr_t2003, speech_protection_kernel__harm_threshold_reading, theater_ratio, 2003, 0.25).
narrative_ontology:measurement_basis(spee_tr_t2003, observed).
narrative_ontology:measurement(spee_tr_t2015, speech_protection_kernel__harm_threshold_reading, theater_ratio, 2015, 0.28).
narrative_ontology:measurement_basis(spee_tr_t2015, observed).
narrative_ontology:measurement(spee_tr_t2026, speech_protection_kernel__harm_threshold_reading, theater_ratio, 2026, 0.31).
narrative_ontology:measurement_basis(spee_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(spee_be_t1965, speech_protection_kernel__harm_threshold_reading, base_extractiveness, 1965, 0.42).
narrative_ontology:measurement_basis(spee_be_t1965, observed).
narrative_ontology:measurement(spee_be_t1978, speech_protection_kernel__harm_threshold_reading, base_extractiveness, 1978, 0.46).
narrative_ontology:measurement_basis(spee_be_t1978, observed).
narrative_ontology:measurement(spee_be_t1991, speech_protection_kernel__harm_threshold_reading, base_extractiveness, 1991, 0.5).
narrative_ontology:measurement_basis(spee_be_t1991, observed).
narrative_ontology:measurement(spee_be_t2003, speech_protection_kernel__harm_threshold_reading, base_extractiveness, 2003, 0.53).
narrative_ontology:measurement_basis(spee_be_t2003, observed).
narrative_ontology:measurement(spee_be_t2015, speech_protection_kernel__harm_threshold_reading, base_extractiveness, 2015, 0.57).
narrative_ontology:measurement_basis(spee_be_t2015, observed).
narrative_ontology:measurement(spee_be_t2026, speech_protection_kernel__harm_threshold_reading, base_extractiveness, 2026, 0.6).
narrative_ontology:measurement_basis(spee_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(spee_su_t1965, speech_protection_kernel__harm_threshold_reading, suppression_requirement, 1965, 0.36).
narrative_ontology:measurement_basis(spee_su_t1965, observed).
narrative_ontology:measurement(spee_su_t1978, speech_protection_kernel__harm_threshold_reading, suppression_requirement, 1978, 0.42).
narrative_ontology:measurement_basis(spee_su_t1978, observed).
narrative_ontology:measurement(spee_su_t1991, speech_protection_kernel__harm_threshold_reading, suppression_requirement, 1991, 0.47).
narrative_ontology:measurement_basis(spee_su_t1991, observed).
narrative_ontology:measurement(spee_su_t2003, speech_protection_kernel__harm_threshold_reading, suppression_requirement, 2003, 0.52).
narrative_ontology:measurement_basis(spee_su_t2003, observed).
narrative_ontology:measurement(spee_su_t2015, speech_protection_kernel__harm_threshold_reading, suppression_requirement, 2015, 0.56).
narrative_ontology:measurement_basis(spee_su_t2015, observed).
narrative_ontology:measurement(spee_su_t2026, speech_protection_kernel__harm_threshold_reading, suppression_requirement, 2026, 0.6).
narrative_ontology:measurement_basis(spee_su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_protection_kernel__harm_threshold_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(speech_protection_kernel__harm_threshold_reading, speech_protection_kernel__absolutist_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__harm_threshold_reading, speech_protection_kernel__marketplace_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__harm_threshold_reading, speech_protection_kernel__dignity_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__harm_threshold_reading, speech_protection_kernel__democratic_participation_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'freedom of speech' conflates five structurally distinct commitments that share one constitutional kernel (fixed rights-protective texts licensing enumerated restrictions). Per the epsilon-invariance principle, each reading is authored as its own file with its own epsilon, victim set, and classification; this file instantiates the harm-threshold reading. Structural relations authored in cs_structure.reading_relations: this reading logically forecloses the absolutist reading (conditional-on-harm and categorical-protection premises cannot coexist in one framework) and coexists conjunctively with the dignity and democratic-participation readings, which modern speech codes routinely hold together with this one. Downstream pressure on the marketplace reading: harm-based restriction removes falsehoods by deletion rather than rebuttal, degrading the operating conditions the counterspeech mechanism presupposes.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
