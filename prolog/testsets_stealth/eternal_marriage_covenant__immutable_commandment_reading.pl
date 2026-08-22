% ============================================================================
% CONSTRAINT STORY: eternal_marriage_covenant__immutable_commandment_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_eternal_marriage_covenant__immutable_commandment_reading, []).

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
 *   constraint_id: eternal_marriage_covenant__immutable_commandment_reading
 *   human_readable: Plural Marriage as Eternal Exaltation Requirement — Immutable Commandment Reading of D&C 132
 *   domain: religious_law/political_theology/commitment_system_dynamics
 *
 * SUMMARY:
 *   D&C 132, recorded in 1843 and publicly announced in 1852, is read here as
 *   verbatim eternal statute: plural marriage is required for the highest
 *   degree of exaltation, no living prophet may supersede it, and no earthly
 *   law releases anyone from it. Under this reading the arrangement binds
 *   every member of the covenant community to assent, binds a subset of elite
 *   men and their wives to practice, and — once the Morrill, Edmunds, and
 *   Edmunds-Tucker machinery activates — places the whole community in a
 *   forced choice in which compliance with the state reads as apostasy and
 *   compliance with the covenant reads as crime. The reading admits no
 *   revision path by definition, which is why federal pressure produces
 *   martyrdom rather than adaptation inside this frame; adaptation happens
 *   only when holders migrate to a sibling reading or out of the tradition
 *   entirely. This file instantiates ONE reading of the
 *   eternal_marriage_covenant kernel; the prophetic_override_reading and
 *   temporal_accommodation_reading are separate constraints with their own
 *   epsilon values, victim sets, and revision paths, authored elsewhere and
 *   linked through the network section. The claimed type and the metrics
 *   below are independent authored facts: I claim tangled_rope because the
 *   arrangement demonstrably coordinates a real covenant community while
 *   extracting asymmetrically from identifiable seats under active
 *   enforcement; the engine computes per-seat classifications from the
 *   structural data, and any divergence between my claim and the computed
 *   types is the measurement this corpus exists to take. KEY AGENTS (by
 *   structural relationship): - first_presidency_and_quorum_of_twelve:
 *   agenda_setter (institutional/identity_locked) — holds the sealing keys,
 *   administers discipline, cannot repudiate the text without dissolving
 *   their own warrant - plural_wives: primary target
 *   (powerless/identity_locked) — bear the domestic, reproductive, and legal
 *   costs - plural_marriage_elite_men: principal beneficiary
 *   (powerful/identity_locked) — convert obedience into standing, alliances,
 *   and household labor - rank_and_file_covenant_members: net payer with
 *   incidental benefit (moderate/constrained) — carry prosecution,
 *   disfranchisement, and asset-seizure costs - dissenting_latter_day_saints:
 *   excluded objectors (moderate/trapped) — severed before the conversation
 *   closes - united_states_federal_authorities: external adversary-observer
 *   (institutional/analytical) — sets the external price of the practice -
 *   post_manifesto_principled_holdouts: schismatic continuators
 *   (organized/identity_locked) — pay fellowship itself to keep practicing
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(eternal_marriage_covenant__immutable_commandment_reading, 0.86).
domain_priors:suppression_score(eternal_marriage_covenant__immutable_commandment_reading, 0.88).
domain_priors:theater_ratio(eternal_marriage_covenant__immutable_commandment_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(eternal_marriage_covenant__immutable_commandment_reading, extractiveness, 0.86).
narrative_ontology:constraint_metric(eternal_marriage_covenant__immutable_commandment_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(eternal_marriage_covenant__immutable_commandment_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(eternal_marriage_covenant__immutable_commandment_reading, accessibility_collapse, 0.76).
narrative_ontology:constraint_metric(eternal_marriage_covenant__immutable_commandment_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(eternal_marriage_covenant__immutable_commandment_reading, tangled_rope).
narrative_ontology:human_readable(eternal_marriage_covenant__immutable_commandment_reading, "Plural Marriage as Eternal Exaltation Requirement — Immutable Commandment Reading of D&C 132").
narrative_ontology:topic_domain(eternal_marriage_covenant__immutable_commandment_reading, "religious_law/political_theology/commitment_system_dynamics").

domain_priors:requires_active_enforcement(eternal_marriage_covenant__immutable_commandment_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(eternal_marriage_covenant__immutable_commandment_reading, 'aa7ab986-eb15-4fee-984c-1aed16395f75').
narrative_ontology:cs_kernel_codification('aa7ab986-eb15-4fee-984c-1aed16395f75', fixed_text).
narrative_ontology:cs_authority_grounding('aa7ab986-eb15-4fee-984c-1aed16395f75', lineage).
narrative_ontology:cs_interpretation_layer_present('aa7ab986-eb15-4fee-984c-1aed16395f75').
narrative_ontology:cs_reading_relation('aa7ab986-eb15-4fee-984c-1aed16395f75', eternal_marriage_covenant__prophetic_override_reading, forecloses).
narrative_ontology:cs_reading_relation('aa7ab986-eb15-4fee-984c-1aed16395f75', eternal_marriage_covenant__temporal_accommodation_reading, forecloses).
narrative_ontology:cs_axiom('aa7ab986-eb15-4fee-984c-1aed16395f75', foundational, plural_marriage_required_for_exaltation).
narrative_ontology:cs_axiom_status(plural_marriage_required_for_exaltation, holdable).
narrative_ontology:cs_axiom_grounding('aa7ab986-eb15-4fee-984c-1aed16395f75', plural_marriage_required_for_exaltation, theological).
narrative_ontology:cs_axiom('aa7ab986-eb15-4fee-984c-1aed16395f75', foundational, eternal_commandment_admits_no_override_or_suspension).
narrative_ontology:cs_axiom_status(eternal_commandment_admits_no_override_or_suspension, holdable).
narrative_ontology:cs_axiom_grounding('aa7ab986-eb15-4fee-984c-1aed16395f75', eternal_commandment_admits_no_override_or_suspension, theological).
narrative_ontology:cs_reference_frame('aa7ab986-eb15-4fee-984c-1aed16395f75', dc132_verbatim_eternal_statute).
narrative_ontology:cs_drift_state('aa7ab986-eb15-4fee-984c-1aed16395f75', second_manifesto_era, gap(codification_collapse, severe, true)).
narrative_ontology:cs_created_at('aa7ab986-eb15-4fee-984c-1aed16395f75', '').
narrative_ontology:cs_kernel_id(eternal_marriage_covenant__immutable_commandment_reading, eternal_marriage_covenant).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(eternal_marriage_covenant__immutable_commandment_reading, first_presidency_and_quorum_of_twelve).
narrative_ontology:constraint_beneficiary(eternal_marriage_covenant__immutable_commandment_reading, plural_marriage_elite_men).
narrative_ontology:constraint_victim(eternal_marriage_covenant__immutable_commandment_reading, plural_wives).
narrative_ontology:constraint_victim(eternal_marriage_covenant__immutable_commandment_reading, rank_and_file_covenant_members).
narrative_ontology:constraint_victim(eternal_marriage_covenant__immutable_commandment_reading, dissenting_latter_day_saints).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(eternal_marriage_covenant__immutable_commandment_reading, rank_and_file_covenant_members).
narrative_ontology:constraint_beneficiary(eternal_marriage_covenant__immutable_commandment_reading, post_manifesto_principled_holdouts).
narrative_ontology:constraint_victim(eternal_marriage_covenant__immutable_commandment_reading, post_manifesto_principled_holdouts).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive, interpret, and administer the 1843 revelation; alone hold the sealing keys that make marriages binding beyond death; convene the councils that try members for disobedience; after 1890 issue the announcements that discipline those who continue the practice. Their offices rest on an unbroken succession from the man who received the text, so repudiating it would dissolve the warrant of their own authority; their room to maneuver runs through reinterpretation, never repeal.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__immutable_commandment_reading, first_presidency_and_quorum_of_twelve, agenda_setter,
    institutional, civilizational, identity_locked, continental).

% Bear the domestic and reproductive load of multi-wife households: serial childbirth, a husband's divided time and income, rivalry management, and years of separation when husbands are imprisoned or in hiding. Divorce is procedurally available but severs sealing hopes, children's covenant standing, and social position; many entered under the conviction that refusal forfeits the highest heaven. The Relief Society gives them collective voice in welfare work but not in the councils that define and apply the law.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__immutable_commandment_reading, plural_wives, payer,
    powerless, biographical, identity_locked, regional).

% Senior priesthood holders who take additional wives, gaining household labor, marriage-alliance networks with other leading families, and standing as exemplars of obedience. Several serve penitentiary terms for unlawful cohabitation, which converts their status into demonstrated sacrifice. Their honor inside the community is inseparable from the practice; abandoning it would cost them the very standing the practice built.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__immutable_commandment_reading, plural_marriage_elite_men, beneficiary,
    powerful, generational, identity_locked, regional).

% Ordinary members who accept the law as a condition of full fellowship and hope of exaltation. Most of the second generation never enter plural marriage themselves, yet they tithe to support families of imprisoned husbands, absorb disfranchisement at the polls, and carry the economic shock of church asset seizures. Temple access and membership standing ride on assenting to a practice most will never personally undertake; leaving would mean losing community, kinship, and promised salvation in a single act.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__immutable_commandment_reading, rank_and_file_covenant_members, payer,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(eternal_marriage_covenant__immutable_commandment_reading, rank_and_file_covenant_members, beneficiary).

% Members who question the law's necessity or its application — some drift into the Godbeite reform circle, some quietly refuse a called marriage — and are brought before council, released from callings, or cut off. Severance removes them from the conversation in which the law is defended and applied; their objections survive only in letters, court testimony, and the records of the splinter movements they founded.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__immutable_commandment_reading, dissenting_latter_day_saints, excluded,
    moderate, biographical, trapped, regional).

% Congress, the federal courts, and territorial prosecutors treat plural marriage as a bar to statehood and republican self-government: the Morrill, Edmunds, and Edmunds-Tucker measures, the Reynolds decision, hundreds of jailed husbands, and the seizure and escheatment of church property. They sit wholly outside the covenant's internal conversation; their leverage determines what the practice costs, not what it teaches.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__immutable_commandment_reading, united_states_federal_authorities, observer,
    institutional, generational, analytical, national).

% Families who conclude that the 1890 announcement cannot bind what God decreed eternally, and continue marrying plurally in northern Mexico, southern Alberta, and the underground. After 1904 they are excommunicated for it: they lose fellowship, temple access, and legal safety, and gain small colonized communities where the practice remains the local norm and the price of belonging.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__immutable_commandment_reading, post_manifesto_principled_holdouts, payer,
    organized, generational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(eternal_marriage_covenant__immutable_commandment_reading, post_manifesto_principled_holdouts, beneficiary).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(eternal_marriage_covenant__immutable_commandment_reading, first_presidency_and_quorum_of_twelve).
narrative_ontology:fixing_cost_class(eternal_marriage_covenant__immutable_commandment_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Binds a scattered, embattled people into a single covenant community through eternal sealing networks; solves the commitment problem of who will sacrifice for Zion by making the sacrifice itself the membership test; organizes household formation, mutual aid among plural families, and demographic expansion in isolated settlements.
% TRANSFER_FUNCTION: Moves domestic labor, reproductive capacity, household resources, and obedience from women and rank-and-file members toward patriarchal households and the presiding hierarchy; after 1862 it also moves legal jeopardy, disfranchisement, and property loss onto the whole membership; it concentrates sole authority to confer the exaltation-requisite ordinance in the hands of the sealing-holding priesthood.
% ABSENT_VOICES: Plural wives sat outside the councils that defined and applied the law — the Relief Society deliberated under presidency direction and never adjudicated the doctrine itself. Dissenters were severed before positions hardened, so the recorded unanimity of conference votes partly reflects who was still in the room. Young women given in marriage had no formal voice at all; their consent is reconstructed from diaries, not minutes.
% DISAPPEARANCE_RATIONALE: If the covenant vanished overnight, the sealing-based kinship network, the loyalty machinery built on costly sacrifice, the distinctiveness that separated the gathered people from surrounding America, and the entire martyrdom economy would dissolve at once. Utah society would reorganize around monogamous nuclear households within a generation, the hierarchy would lose its costliest commitment instrument, and the schismatic colonies would lose their reason for existing.
% FOUNDING_PROBLEM: Gather and retain a covenant people set apart from Babylon: bind generations together through eternal sealing, raise up a righteous posterity, and forge a community whose members have demonstrably sacrificed everything for Zion — solving the commitment problem of a gathered remnant in hostile territory.
% FOUNDING_PROBLEM_CORROBORATION: No source outside the benefiting parties attests that the salvific problem remains live — the reading's own holders and their fundamentalist heirs are the only attestors, which is itself signal. Historians, court records, and prison registers corroborate the sincerity of the belief and the reality of the costs borne; they do not corroborate the liveness of the exaltation problem the belief answers.
narrative_ontology:disappearance_verdict(eternal_marriage_covenant__immutable_commandment_reading, world_rearranges).
narrative_ontology:founding_problem_status(eternal_marriage_covenant__immutable_commandment_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(eternal_marriage_covenant__immutable_commandment_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(eternal_marriage_covenant__immutable_commandment_reading, 'none', 1).
narrative_ontology:epsilon_provenance(eternal_marriage_covenant__immutable_commandment_reading, 0.86, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(eternal_marriage_covenant__immutable_commandment_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(eternal_marriage_covenant__immutable_commandment_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(eternal_marriage_covenant__immutable_commandment_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction ends at 0.86 because by the interval's end the arrangement stacks three cost layers on its governed seats simultaneously: the intrinsic domestic and reproductive burdens borne overwhelmingly by plural wives, the legal layer of imprisonment and property forfeiture imposed after 1862, and the fellowship layer after 1890–1904 in which holding the reading costs membership in the main body itself. Suppression ends at 0.88 because enforcement doubles rather than decays: state prosecution intensifies through the Edmunds era, and after the Second Manifesto the church's own disciplinary machinery turns against practitioners, so holders face suppression from both directions at once. Suppression is authored as a raw structural property and is NOT scaled by power or scope — only extraction is scaled, by the engine, through directionality and scope. Theater stays low throughout (0.12 to 0.28) because the practice was genuinely costly — men went to prison for it, women bore its consequences for life — but it rises modestly after 1890 as covert solemnization and official denial add an appearance-management layer atop the functional practice. Accessibility_collapse is high (0.76) because inside the framework refusal forfeits exaltation outright and no substitute ordinance exists; the collapse is internal to the frame, since physical exit remained possible at the price of everything the frame counts as valuable. Resistance is substantial (0.62): internal dissent (Godbeites, severances, women's quiet refusals) plus the sustained external assault of Congress, the courts, and the anti-polygamy press. All three tracked series share one time grid (points 0, 10, 20, 30, 40, 52 mapping 1852 through the Second Manifesto era) so no metric is sampled against another metric's end-state value.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute very differently. From the hierarchy's position the arrangement is the dispensational order they steward: the price of continuity with the founding revelation and the mechanism that proves who is willing to sacrifice for Zion. From the plural wives' position the same structure is a lifetime of divided provision, serial confinement, and legal jeopardy they did not legislate. From the rank-and-file position it is a salvific ticket they mostly never redeem personally but must affirm to keep temple access. From the federal observer's position it is civic rebellion dressed in liturgy. The engine derives these divergent per-seat classifications from the structural data — power atoms, exit options, and directionalities — not from the authored claim.
 *
 * DIRECTIONALITY LOGIC:
 *   The hierarchy and the elite patriarchs sit near the beneficiary end: they collect authority concentration, household labor, alliance networks, and demonstrated-obedience standing, and their identity_locked exits mean the arrangement subsidizes precisely those least able to leave it. Plural wives sit nearest the full-target end: they bear the largest direct transfer, their exit is identity_locked (severed sealings, children's covenant standing, social death), and their power atom is the lowest in the story. Rank-and-file members derive high directionality as net payers — prosecution exposure, disfranchisement, tithing diverted to prisoners' families — despite the incidental exaltation benefit their secondary role records. Dissenters are full targets until expulsion removes them from the governed set entirely. The federal authorities sit analytically outside the arrangement's gain-and-cost flow: they impose costs on it rather than receiving from it.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification is what keeps both faces of this arrangement visible at once. Calling it pure coordination erases the wives' ledger — decades of uncompensated domestic and reproductive transfer and the post-1862 criminalization of the governed. Calling it pure extraction erases the real covenant community it built, the sincere conviction that sustained it, and the mutual-aid networks that functioned under siege. On mandatrophy: inside this reading the founding problem is eternally live by definition, so the arrangement cannot age into obsolescence on its own terms — there is no sunset clause and the reading forbids writing one. What actually happened is that the parent institution resolved ITS mandate question in 1890–1904 by flipping enforcement against the practice, which did not retire the constraint but migrated it: the reading survived intact in schismatic colonies where it remains fully operational and costly. A piton misread would predict theatrical maintenance and diffuse costs with no concentrated bearer; the observed record shows the opposite — low theater, concentrated suffering, and administrators who paid prison terms rather than revise. The cost-asymmetry test confirms this is no piton: the hierarchy could have revised, but the cost to them (dissolution of their own authority warrant) exceeded anything they bore, so they held until external force broke the frame for everyone who stayed inside it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest_location,
    'This constraint is one reading of the eternal_marriage_covenant kernel; would instantiating the prophetic_override_reading or the temporal_accommodation_reading instead change the constraint''s enforcement structure, revision paths, and victim sets?',
    'Author the sibling readings as separate stories and compare computed classifications; the disagreement is located in whether D&C 132''s force is categorical or defeasible, and each sibling resolves that element differently.',
    'Under either sibling reading the no-revision-path property disappears, the martyrdom dynamic dissolves, and the suppression profile drops sharply; this file''s classification holds only for the immutable reading and must not be generalized to the kernel.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest_location, conceptual, 'Committer structure: which reading of the sealing kernel this constraint instantiates and what the siblings would change.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the measured suppression of covenant members structural (ecclesiastical discipline, severance, social death, prosecution) or internalized (settled conviction that refusal forfeits exaltation)?',
    'Post-exit trajectory of those who left for the Reorganized church or for secular life: if fear of forfeited exaltation and severed-sealing anxiety persist after removal from all enforcement reach, classify the residual as internalized.',
    'If largely internalized, effective suppression exceeds the structural measure and exit stays closed even where enforcement lapses; if largely structural, enforcement decay would open exit far faster than the historical record shows it opening.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Structural versus internalized suppression mechanism in a totalizing covenant community.').

omega_variable(
    consent_under_collapsed_alternatives,
    'How much of the burden borne by plural wives — particularly the youngest — reflects meaningful consent versus choice made inside a framework where refusal forfeits exaltation, family standing, and children''s covenant status?',
    'Systematic reading of women''s diaries, correspondence, and divorce petitions against the counterfactual option set actually available to each woman at the time of her sealing.',
    'A low-consent finding raises effective extraction above the authored value and strengthens target-side classification for the wife seats; a high-consent finding supports treating part of the burden as accepted sacrificial cost within the framework''s own accounting.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consent_under_collapsed_alternatives, empirical, 'Whether apparent consent survives the framework''s collapse of alternatives.').

omega_variable(
    martyrdom_premium_attribution,
    'Does the rise in extraction between 1862 and 1904 belong to the covenant arrangement itself or to the federal enforcement layered on top of it?',
    'Compare cost profiles of covenant communities before the Morrill Act and after the Second Manifesto in the northern Mexico colonies, where federal enforcement never effectively reached.',
    'If colonial cost profiles stay high without federal pressure, the extraction is intrinsic to the arrangement; if they fall markedly, the measured rise is state-imposed and the arrangement''s intrinsic extraction sits nearer its 1852 level.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(martyrdom_premium_attribution, conceptual, 'Attribution of the persecution-era extraction premium between the covenant and the state.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(eternal_marriage_covenant__immutable_commandment_reading, 0, 52).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eter_tr_t0, eternal_marriage_covenant__immutable_commandment_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(eter_tr_t10, eternal_marriage_covenant__immutable_commandment_reading, theater_ratio, 10, 0.13).
narrative_ontology:measurement(eter_tr_t20, eternal_marriage_covenant__immutable_commandment_reading, theater_ratio, 20, 0.15).
narrative_ontology:measurement(eter_tr_t30, eternal_marriage_covenant__immutable_commandment_reading, theater_ratio, 30, 0.18).
narrative_ontology:measurement(eter_tr_t40, eternal_marriage_covenant__immutable_commandment_reading, theater_ratio, 40, 0.24).
narrative_ontology:measurement(eter_tr_t52, eternal_marriage_covenant__immutable_commandment_reading, theater_ratio, 52, 0.28).

% Extraction over time
narrative_ontology:measurement(eter_be_t0, eternal_marriage_covenant__immutable_commandment_reading, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(eter_be_t10, eternal_marriage_covenant__immutable_commandment_reading, base_extractiveness, 10, 0.64).
narrative_ontology:measurement(eter_be_t20, eternal_marriage_covenant__immutable_commandment_reading, base_extractiveness, 20, 0.7).
narrative_ontology:measurement(eter_be_t30, eternal_marriage_covenant__immutable_commandment_reading, base_extractiveness, 30, 0.8).
narrative_ontology:measurement(eter_be_t40, eternal_marriage_covenant__immutable_commandment_reading, base_extractiveness, 40, 0.84).
narrative_ontology:measurement(eter_be_t52, eternal_marriage_covenant__immutable_commandment_reading, base_extractiveness, 52, 0.86).

% Suppression requirement over time
narrative_ontology:measurement(eter_su_t0, eternal_marriage_covenant__immutable_commandment_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(eter_su_t10, eternal_marriage_covenant__immutable_commandment_reading, suppression_requirement, 10, 0.5).
narrative_ontology:measurement(eter_su_t20, eternal_marriage_covenant__immutable_commandment_reading, suppression_requirement, 20, 0.62).
narrative_ontology:measurement(eter_su_t30, eternal_marriage_covenant__immutable_commandment_reading, suppression_requirement, 30, 0.78).
narrative_ontology:measurement(eter_su_t40, eternal_marriage_covenant__immutable_commandment_reading, suppression_requirement, 40, 0.84).
narrative_ontology:measurement(eter_su_t52, eternal_marriage_covenant__immutable_commandment_reading, suppression_requirement, 52, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(eternal_marriage_covenant__immutable_commandment_reading, identity_coordination).
narrative_ontology:affects_constraint(eternal_marriage_covenant__immutable_commandment_reading, eternal_marriage_covenant__prophetic_override_reading).
narrative_ontology:affects_constraint(eternal_marriage_covenant__immutable_commandment_reading, eternal_marriage_covenant__temporal_accommodation_reading).
narrative_ontology:affects_constraint(eternal_marriage_covenant__immutable_commandment_reading, federal_antipolygamy_enforcement_regime).

% DUAL FORMULATION NOTE:
% The colloquial label 'the Mormon polygamy doctrine' conflates three structurally distinct constraints that share one canonized text (kernel eternal_marriage_covenant). This file is the immutable_commandment_reading: its epsilon is authored for the arrangement in which the text binds absolutely and no revision path exists. The prophetic_override_reading and the temporal_accommodation_reading instantiate different enforcement structures, revision paths, and victim sets over the same text and are authored as separate stories. Structural ordering: the immutable reading is the baseline from which the other two diverge under federal pressure — the sibling readings exist precisely as escape routes from this one — so this story links downstream to both, and to the federal enforcement regime whose pressure drove the divergence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
