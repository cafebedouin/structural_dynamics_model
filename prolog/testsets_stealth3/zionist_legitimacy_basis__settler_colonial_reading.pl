% ============================================================================
% CONSTRAINT STORY: zionist_legitimacy_basis__settler_colonial_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_zionist_legitimacy_basis__settler_colonial_reading, []).

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
 *   constraint_id: zionist_legitimacy_basis__settler_colonial_reading
 *   human_readable: Zionist Ethno-State Arrangement (Settler-Colonial Reading)
 *   domain: political_history/nationalism/settler_colonial_studies
 *
 * SUMMARY:
 *   This story authors ONE reading of the contested kernel
 *   zionist_legitimacy_basis: the settler-colonial reading, on which the
 *   Zionist movement was a European settler-colonial project that established
 *   an ethno-state through indigenous displacement, such that the colonial
 *   structure — not the persecution narrative or any theological warrant —
 *   determines the arrangement's legitimacy, and displacement is constitutive
 *   rather than incidental. The epsilon referent is the standing arrangement
 *   under contest: the actually-existing state structure built by the
 *   movement (Law of Return, land regime, occupation administration, denial
 *   of refugee return), assessed by this reading's own lights. The sibling
 *   readings (national_liberation_reading, religious_restoration_reading) are
 *   separate constraints in other files; they are not averaged in, hedged
 *   against, or described inside this one. The claimed type (snare) and the
 *   metrics are independently authored: the reading holds the
 *   refuge-and-democracy coordination narrative functions substantially as
 *   legitimation cover for a structure whose persistence depends on coercion
 *   and on suppressing exits (return denied, blockade, fragmented
 *   jurisdiction), with identifiable victims across four Palestinian seats.
 *   The engine computes per-seat classifications from the structural data;
 *   divergence between seats is the measurement, not an error to reconcile.
 *
 * KEY AGENTS:
 *   - - israeli_state_apparatus: Agenda setter (institutional/arbitrage) — administers land, citizenship, and occupation; alone able to restructure the arrangement
 *   - - jewish_israeli_citizenry: Primary beneficiary (organized/constrained) — receives land, housing, security, identity-expressive state; pays conscription and isolation
 *   - - diaspora_jewish_communities: Secondary beneficiary (organized/mobile) — collects refuge and identity anchor at zero direct cost
 *   - - us_strategic_patronage_network: Institutional beneficiary (institutional/arbitrage) — supplies enforcement-underwriting aid and shielding, collects alliance value
 *   - - palestinian_citizens_of_israel: Target seat (moderate/trapped) — formal inclusion, structural subordination
 *   - - west_bank_palestinians: Primary target (powerless/trapped) — military administration, no citizenship in governing state
 *   - - gaza_residents: Primary target (powerless/trapped) — blockade confinement, no exit in any direction
 *   - - palestinian_refugee_diaspora: Primary target (powerless/trapped) — multi-generational denial of return, host-state precarity
 *   - - binational_democracy_advocates: Excluded voice (moderate/constrained) — alternative kept outside the feasible set
 *   - - international_legal_bodies: Analytical observer (institutional/analytical) — documents and adjudicates without enforcement force
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(zionist_legitimacy_basis__settler_colonial_reading, 0.92).
domain_priors:suppression_score(zionist_legitimacy_basis__settler_colonial_reading, 0.9).
domain_priors:theater_ratio(zionist_legitimacy_basis__settler_colonial_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(zionist_legitimacy_basis__settler_colonial_reading, extractiveness, 0.92).
narrative_ontology:constraint_metric(zionist_legitimacy_basis__settler_colonial_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(zionist_legitimacy_basis__settler_colonial_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(zionist_legitimacy_basis__settler_colonial_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(zionist_legitimacy_basis__settler_colonial_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(zionist_legitimacy_basis__settler_colonial_reading, snare).
narrative_ontology:human_readable(zionist_legitimacy_basis__settler_colonial_reading, "Zionist Ethno-State Arrangement (Settler-Colonial Reading)").
narrative_ontology:topic_domain(zionist_legitimacy_basis__settler_colonial_reading, "political_history/nationalism/settler_colonial_studies").

domain_priors:requires_active_enforcement(zionist_legitimacy_basis__settler_colonial_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(zionist_legitimacy_basis__settler_colonial_reading, '5cf81b49-63e6-4b26-8df9-e0409cf81ca1').
narrative_ontology:cs_kernel_codification('5cf81b49-63e6-4b26-8df9-e0409cf81ca1', distributed).
narrative_ontology:cs_authority_grounding('5cf81b49-63e6-4b26-8df9-e0409cf81ca1', distributed).
narrative_ontology:cs_reading_relation('5cf81b49-63e6-4b26-8df9-e0409cf81ca1', zionist_legitimacy_basis__national_liberation_reading, forecloses).
narrative_ontology:cs_reading_relation('5cf81b49-63e6-4b26-8df9-e0409cf81ca1', zionist_legitimacy_basis__religious_restoration_reading, coexists_with).
narrative_ontology:cs_axiom('5cf81b49-63e6-4b26-8df9-e0409cf81ca1', foundational, displacement_constitutes_the_movement).
narrative_ontology:cs_axiom_status(displacement_constitutes_the_movement, holdable).
narrative_ontology:cs_axiom_grounding('5cf81b49-63e6-4b26-8df9-e0409cf81ca1', displacement_constitutes_the_movement, empirically_contingent).
narrative_ontology:cs_axiom('5cf81b49-63e6-4b26-8df9-e0409cf81ca1', foundational, colonial_structure_determines_legitimacy).
narrative_ontology:cs_axiom_status(colonial_structure_determines_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('5cf81b49-63e6-4b26-8df9-e0409cf81ca1', colonial_structure_determines_legitimacy, deontological).
narrative_ontology:cs_axiom('5cf81b49-63e6-4b26-8df9-e0409cf81ca1', secondary, persecution_does_not_confer_displacement_rights).
narrative_ontology:cs_axiom_status(persecution_does_not_confer_displacement_rights, holdable).
narrative_ontology:cs_axiom_grounding('5cf81b49-63e6-4b26-8df9-e0409cf81ca1', persecution_does_not_confer_displacement_rights, deontological).
narrative_ontology:cs_reference_frame('5cf81b49-63e6-4b26-8df9-e0409cf81ca1', comparative_settler_colonial_paradigm).
narrative_ontology:cs_drift_state('5cf81b49-63e6-4b26-8df9-e0409cf81ca1', contemporary_post_2023_escalation, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('5cf81b49-63e6-4b26-8df9-e0409cf81ca1', '').
narrative_ontology:cs_kernel_id(zionist_legitimacy_basis__settler_colonial_reading, zionist_legitimacy_basis).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(zionist_legitimacy_basis__settler_colonial_reading, jewish_israeli_citizenry).
narrative_ontology:constraint_beneficiary(zionist_legitimacy_basis__settler_colonial_reading, diaspora_jewish_communities).
narrative_ontology:constraint_beneficiary(zionist_legitimacy_basis__settler_colonial_reading, us_strategic_patronage_network).
narrative_ontology:constraint_victim(zionist_legitimacy_basis__settler_colonial_reading, palestinian_citizens_of_israel).
narrative_ontology:constraint_victim(zionist_legitimacy_basis__settler_colonial_reading, west_bank_palestinians).
narrative_ontology:constraint_victim(zionist_legitimacy_basis__settler_colonial_reading, gaza_residents).
narrative_ontology:constraint_victim(zionist_legitimacy_basis__settler_colonial_reading, palestinian_refugee_diaspora).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(zionist_legitimacy_basis__settler_colonial_reading, jewish_israeli_citizenry).
narrative_ontology:constraint_vindicates(zionist_legitimacy_basis__settler_colonial_reading, demographic_jewish_majority_doctrine).
narrative_ontology:constraint_vindicates(zionist_legitimacy_basis__settler_colonial_reading, law_of_return_exclusivity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the state's land registry, citizenship law, and military government over the occupied territories. Sets settlement policy, operates the Law of Return admitting any person with one Jewish grandparent, and maintains the legal architecture that bars displaced Palestinians and their descendants from returning. Of all parties, it alone can restructure the arrangement — withdraw, annex, or equalize citizenship — and it sets the terms on which everyone else experiences it.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__settler_colonial_reading, israeli_state_apparatus, agenda_setter,
    institutional, generational, arbitrage, regional).

% Receives the arrangement's outputs: housing on state and expropriated land, first-tier infrastructure and water allocation, military protection, and a state whose symbols and institutions express its majority's identity. Pays through universal conscription, war casualties, taxation, and mounting international isolation. Emigration is legally open but severing family, community, and livelihood makes it costly for most.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__settler_colonial_reading, jewish_israeli_citizenry, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(zionist_legitimacy_basis__settler_colonial_reading, jewish_israeli_citizenry, payer).

% Hold a guaranteed refuge and an identity anchor in the state. Fund it through donations and bond purchases, defend it through advocacy inside host-country politics, and can disengage at will. They bear none of the displacement's direct costs and experience the arrangement almost entirely through its refuge and meaning functions.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__settler_colonial_reading, diaspora_jewish_communities, beneficiary,
    organized, generational, mobile, global).

% Supplies the military aid, diplomatic shielding, and intelligence cooperation that underwrite the arrangement's enforcement capacity, and receives in return basing access, a dependable regional partner, and domestic coalition value. Much of the aid recycles into its own defense procurement. It can redirect or condition support at lower cost than any regional party, and periodically threatens to.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__settler_colonial_reading, us_strategic_patronage_network, beneficiary,
    institutional, generational, arbitrage, continental).

% Hold formal citizenship and vote, roughly a fifth of the population, but live under a legal order that defines the state as belonging to another people. Families displaced internally in 1948 remain on a registry of absentees barred from their former property; planning authorities deny most unrecognized villages expansion permits; the nation-state law demotes their language and constitutional standing. Leaving would mean abandoning the only homeland they have.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__settler_colonial_reading, palestinian_citizens_of_israel, payer,
    moderate, biographical, trapped, national).

% Live under military administration: jurisdictional division cuts them off from farmland and from each other, settlement roads and barriers fragment their towns, water allocation favors the settlements, and military courts try thousands each year. They hold no citizenship in the state that governs them; the Palestinian Authority administers civil affairs under the occupying army's ultimate control.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__settler_colonial_reading, west_bank_palestinians, payer,
    powerless, biographical, trapped, regional).

% Confined behind a blockade controlling air, sea, and most land access. Repeated escalations destroy housing, water, and power infrastructure faster than reconstruction restores it. They have no army, no recognized sovereignty, and borders closed in every direction; survival is organized around rationed electricity, tunnel economies, and aid convoys.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__settler_colonial_reading, gaza_residents, payer,
    powerless, immediate, trapped, local).

% Descendants of the 1948 and 1967 displacements, registered with UNRWA across Lebanon, Syria, Jordan, and beyond. They inherit a right of return that the governing state denies and that most host states neutralize by withholding citizenship. Camps are now multi-generational; legal status stays precarious; the claim stays alive precisely because exit was never offered.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__settler_colonial_reading, palestinian_refugee_diaspora, payer,
    powerless, generational, trapped, global).

% Organize for a single state with equal citizenship for both peoples. Every party administering or negotiating the arrangement treats their proposal as outside the feasible set, and they hold no seat in any negotiating forum; their influence runs through academic publication and activist networks rather than decision channels.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__settler_colonial_reading, binational_democracy_advocates, excluded,
    moderate, generational, constrained, global).

% Issue advisory opinions, commission inquiries, and pass resolutions on the occupation, settlement legality, and refugee return. They document and adjudicate but command no enforcement force of their own; their findings enter the record while the parties on the ground continue as before.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__settler_colonial_reading, international_legal_bodies, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(zionist_legitimacy_basis__settler_colonial_reading, jewish_israeli_citizenry).
narrative_ontology:fixing_cost_class(zionist_legitimacy_basis__settler_colonial_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The arrangement solves a real problem for its beneficiaries: it concentrates a scattered, persecuted minority into a sovereign majority, coordinating immigration, defense, land allocation, and institution-building that no diaspora community could provide alone. It delivers refuge capacity and collective security that its beneficiaries genuinely consume.
% TRANSFER_FUNCTION: Moves land, water, mobility rights, and political sovereignty from Palestinians to the Jewish citizenry and state — through 1948 flight and expulsion, absentee-property seizure, ongoing settlement expansion, and occupation administration — and moves diaspora capital and great-power aid inward to sustain the structure.
% ABSENT_VOICES: The displaced themselves: refugees barred from return were absent when the arrangement was constituted in 1947-49 and remain structurally absent from final-status decisions; Gaza residents under blockade have no seat; third-generation camp populations inherit the absence. Binational-equality advocates are excluded from the feasible-set definition. The unanimity of beneficiary-side narratives about the arrangement's legitimacy arises partly because these seats were never in the room.
% DISAPPEARANCE_RATIONALE: If the arrangement vanished overnight, millions of registered refugees would press return claims immediately, regional alliances built around the state would reorganize, the Jewish citizenry would face a security vacuum it has never had to solve without the enforced structure, and every land-title and citizenship question since 1948 would reopen at once.
% FOUNDING_PROBLEM: European antisemitic persecution culminating in the Holocaust left Jews stateless and exposed; the Zionist movement was founded to solve Jewish insecurity through territorial concentration and sovereignty.
% FOUNDING_PROBLEM_CORROBORATION: The founding persecution problem is corroborated from outside the benefiting parties: Holocaust-era refugee records, contemporaneous consular documentation, and persecution historiography attest it. Whether it still grounds the arrangement is disputed across the same external sources: UNRWA registration rolls, ICJ and General Assembly records, and Palestinian oral-history projects attest the displacement side, while Israeli state institutions and major American Jewish organizations attest the insecurity side. Both attestations come from outside the beneficiary set; the dispute over status is real, not manufactured.
narrative_ontology:disappearance_verdict(zionist_legitimacy_basis__settler_colonial_reading, world_rearranges).
narrative_ontology:founding_problem_status(zionist_legitimacy_basis__settler_colonial_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(zionist_legitimacy_basis__settler_colonial_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(zionist_legitimacy_basis__settler_colonial_reading, 'none', 1).
narrative_ontology:epsilon_provenance(zionist_legitimacy_basis__settler_colonial_reading, 0.92, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(zionist_legitimacy_basis__settler_colonial_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(zionist_legitimacy_basis__settler_colonial_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(zionist_legitimacy_basis__settler_colonial_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.92 because, on this reading's assessment of the standing arrangement, land, water, mobility, and sovereignty flow continuously from four Palestinian seats to the beneficiary seats, and the flow accelerated rather than attenuated over the century. Suppression is authored at 0.90 as a raw structural property (unscaled by power or scope — scaling happens only to extractiveness in the engine): the arrangement persists through blockade, military courts, administrative detention, separation infrastructure, and denial of return, not through participant preference. Theater ratio 0.42 reflects a real but partly performative surface: democratic-procedural activity and peace-process diplomacy that legitimates while consolidation proceeds underneath. Accessibility collapse 0.70: return, equal citizenship, and partition-as-agreed have collapsed as practical alternatives for those governed, though they remain alive in discourse. Resistance 0.78: a century of armed, civic, diplomatic, and boycott resistance across every victim seat. The measurement series runs on one shared time grid (all three metrics at all eleven points). The series is CYCLICAL, not monotonic: crisis -> suppression surge -> diplomatic window -> theater spike -> consolidation underneath -> crisis. The 1993 Oslo point shows the signature clearly — theater peaks (0.46) and suppression dips (0.66) while settlement population roughly doubles during the process years. The oscillation is partly an extraction mechanism in the intermittent-reinforcement sense: each diplomatic window reinforces beneficiary-side belief that the conflict is temporary and the arrangement provisional, lowering internal resistance to it, while the underlying land transfer continues. Base_properties values are the interval-end (2025) state.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the beneficiary/agenda-setter seats compute differently from the same structural data. From the state apparatus and citizenry seats the arrangement presents as a protection and self-determination structure they built, staff, and defend — rope-flavored from inside. From the refugee, Gaza, and West Bank seats the same structure presents as enforced dispossession with no exit — snare-flavored. The nominal-inclusion seat (palestinian_citizens_of_israel) sits between: citizenship without belonging. The engine derives these divergent per-seat classifications from the declared roles, exits, and directionalities; this story does not adjudicate which seat's experience is 'the' constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to low directionality: the citizenry receives the arrangement's material outputs directly (land, housing, security); diaspora communities collect refuge and meaning at zero direct cost with mobile exit; the patronage network converts aid flows into alliance value with arbitrage-grade freedom. Victim declarations map to high directionality, ordered by trap depth: the refugee diaspora is nearest the full-target pole (denied return across generations, no host-state integration), then Gaza residents (total confinement), then West Bank Palestinians (military governance without citizenship), then Palestinian citizens of Israel (formal inclusion, structural targeting — the derivation may place them somewhat below the other victim seats on nominal-benefit grounds; their situation text records why the nominal benefit does not reach the structure). The state apparatus is the agenda setter: it administers rather than merely collects, and its restructuring freedom is the largest in the system.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — European Jewish statelessness under exterminatory persecution — was real, externally corroborated, and genuinely solved in its original form: there is no longer a stateless European Jewry facing Nazi-type annihilation. The arrangement persists and now generates the displacement problem it no longer answers to; the founding problem's status is therefore contested rather than dead, because antisemitism persists in transformed form and the beneficiary parties attest its liveness. The mismatch consumer reads founding_problem_status=contested x disappearance_verdict=world_rearranges: the arrangement is load-bearing for the entire regional order, so no zombie flag fires on world-dependence grounds, but the genealogy interview still separates the solved original problem from the persisted structure. Mandatrophy discipline prevents two mislabelings here: it blocks the rope-framing in which the genuine refuge function nets out the displacement cost into a benign coordination ledger (the reading's central objection), and it blocks the piton-framing in which the arrangement would be mostly theatrical inertia — the enforcement is emphatically functional, not performed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'This story instantiates one reading (settler_colonial_reading) of the kernel zionist_legitimacy_basis. Would instantiating the national_liberation_reading instead change the constraint''s victim set, epsilon, and computed type over the identical standing arrangement?',
    'Compile the sibling reading stories and compare computed per-seat classifications and epsilon over the same referent arrangement; the delta between readings is the kernel''s indexical spread.',
    'Under the national-liberation reading the same arrangement computes with a smaller victim set and materially lower epsilon, because the persecution-rescue framing absorbs much of the measured burden as coordination cost. Under this reading displacement is constitutive, the victim set is maximal, and epsilon stays high. Classification of the arrangement is indexical to the reading chosen; neither result is the kernel''s ''true'' value.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Reading-indexicality of the kernel''s classification: committer structure routed here rather than folded into the constraint.').

omega_variable(
    constitutive_or_incidental_displacement,
    'Was Palestinian displacement a planned, constitutive component of the Zionist project (transfer committees, Plan Dalet, systematic village depopulation) or an emergent consequence of war?',
    'Archival programs: declassified IDF archives, village-destruction records, the 1937 Peel-era transfer discussions and 1948 Transfer Committee minutes, land-purchase eviction ledgers, and comparative depopulation timing studies.',
    'A constitutive finding anchors this reading''s foundational axiom and the snare claim. An incidental finding would shift evidentiary weight toward the national-liberation sibling and reclassify residual extraction as tangled-rope overhead on a genuine rescue function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constitutive_or_incidental_displacement, empirical, 'Whether displacement was designed-in or war-emergent; the empirical spine of this reading.').

omega_variable(
    beneficiary_heterogeneity_mizrahi,
    'Do Mizrahi Jews expelled from Arab states count as beneficiaries of the displacement arrangement, or as a second displaced population absorbed into it?',
    'Comparative restitution and absorption histories: Arab-state property-loss registries, absorption-cohort outcome studies, and analyses of the intra-Jewish ethnic hierarchy in land and development-town allocation.',
    'If Mizrahi Jews are co-victims rather than beneficiaries, the beneficiary structure narrows toward the founding cohort and state institutions, raising measured asymmetry and hardening the extraction profile; if beneficiaries, the current declaration stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_heterogeneity_mizrahi, conceptual, 'Heterogeneity inside the beneficiary category complicates the colonizer/indigenous binary this reading relies on.').

omega_variable(
    enforcement_lockin_trajectory,
    'Is the rising suppression trajectory irreversible lock-in (annexation legislation, demographic fait accompli, settlement doubling) or a ratchet that a negotiated settlement could still reverse?',
    'Track settlement-population growth rates, annexation and regularization legislation, third-party recognition shifts, and enforcement-infrastructure investment over the next decade.',
    'Lock-in confirms the exit-suppression structure as permanent and pushes long-run suppression toward ceiling values; demonstrated reversibility would reopen transition-shaped readings and lower projected suppression.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(enforcement_lockin_trajectory, empirical, 'Trajectory uncertainty on the enforcement series'' endpoint.').

omega_variable(
    refuge_function_separability,
    'Could the refuge and self-determination function the arrangement delivers to Jews have been — or yet be — delivered without displacement, through the 1947 partition as accepted, binational statehood, or compensation-plus-return?',
    'Counterfactual historiography on 1947 partition acceptance, uptake analysis of binational proposals, and present-day compensation-and-return modeling feasibility studies.',
    'Separability supports reading the coordination narrative as cover for the extraction structure; inseparability would credit part of the measured burden as genuine coordination cost and leave tangled-rope residue in the classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(refuge_function_separability, conceptual, 'Whether the refuge function and the displacement mechanism are structurally separable.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(zionist_legitimacy_basis__settler_colonial_reading, 1917, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(zlb_scr_tr_t1917, zionist_legitimacy_basis__settler_colonial_reading, theater_ratio, 1917, 0.14).
narrative_ontology:measurement(zlb_scr_tr_t1929, zionist_legitimacy_basis__settler_colonial_reading, theater_ratio, 1929, 0.18).
narrative_ontology:measurement(zlb_scr_tr_t1936, zionist_legitimacy_basis__settler_colonial_reading, theater_ratio, 1936, 0.21).
narrative_ontology:measurement(zlb_scr_tr_t1948, zionist_legitimacy_basis__settler_colonial_reading, theater_ratio, 1948, 0.29).
narrative_ontology:measurement(zlb_scr_tr_t1967, zionist_legitimacy_basis__settler_colonial_reading, theater_ratio, 1967, 0.27).
narrative_ontology:measurement(zlb_scr_tr_t1977, zionist_legitimacy_basis__settler_colonial_reading, theater_ratio, 1977, 0.31).
narrative_ontology:measurement(zlb_scr_tr_t1987, zionist_legitimacy_basis__settler_colonial_reading, theater_ratio, 1987, 0.32).
narrative_ontology:measurement(zlb_scr_tr_t1993, zionist_legitimacy_basis__settler_colonial_reading, theater_ratio, 1993, 0.46).
narrative_ontology:measurement(zlb_scr_tr_t2000, zionist_legitimacy_basis__settler_colonial_reading, theater_ratio, 2000, 0.37).
narrative_ontology:measurement(zlb_scr_tr_t2015, zionist_legitimacy_basis__settler_colonial_reading, theater_ratio, 2015, 0.4).
narrative_ontology:measurement(zlb_scr_tr_t2025, zionist_legitimacy_basis__settler_colonial_reading, theater_ratio, 2025, 0.42).

% Extraction over time
narrative_ontology:measurement(zlb_scr_be_t1917, zionist_legitimacy_basis__settler_colonial_reading, base_extractiveness, 1917, 0.34).
narrative_ontology:measurement(zlb_scr_be_t1929, zionist_legitimacy_basis__settler_colonial_reading, base_extractiveness, 1929, 0.41).
narrative_ontology:measurement(zlb_scr_be_t1936, zionist_legitimacy_basis__settler_colonial_reading, base_extractiveness, 1936, 0.54).
narrative_ontology:measurement(zlb_scr_be_t1948, zionist_legitimacy_basis__settler_colonial_reading, base_extractiveness, 1948, 0.83).
narrative_ontology:measurement(zlb_scr_be_t1967, zionist_legitimacy_basis__settler_colonial_reading, base_extractiveness, 1967, 0.87).
narrative_ontology:measurement(zlb_scr_be_t1977, zionist_legitimacy_basis__settler_colonial_reading, base_extractiveness, 1977, 0.86).
narrative_ontology:measurement(zlb_scr_be_t1987, zionist_legitimacy_basis__settler_colonial_reading, base_extractiveness, 1987, 0.87).
narrative_ontology:measurement(zlb_scr_be_t1993, zionist_legitimacy_basis__settler_colonial_reading, base_extractiveness, 1993, 0.79).
narrative_ontology:measurement(zlb_scr_be_t2000, zionist_legitimacy_basis__settler_colonial_reading, base_extractiveness, 2000, 0.84).
narrative_ontology:measurement(zlb_scr_be_t2015, zionist_legitimacy_basis__settler_colonial_reading, base_extractiveness, 2015, 0.89).
narrative_ontology:measurement(zlb_scr_be_t2025, zionist_legitimacy_basis__settler_colonial_reading, base_extractiveness, 2025, 0.92).

% Suppression requirement over time
narrative_ontology:measurement(zlb_scr_su_t1917, zionist_legitimacy_basis__settler_colonial_reading, suppression_requirement, 1917, 0.22).
narrative_ontology:measurement(zlb_scr_su_t1929, zionist_legitimacy_basis__settler_colonial_reading, suppression_requirement, 1929, 0.3).
narrative_ontology:measurement(zlb_scr_su_t1936, zionist_legitimacy_basis__settler_colonial_reading, suppression_requirement, 1936, 0.52).
narrative_ontology:measurement(zlb_scr_su_t1948, zionist_legitimacy_basis__settler_colonial_reading, suppression_requirement, 1948, 0.71).
narrative_ontology:measurement(zlb_scr_su_t1967, zionist_legitimacy_basis__settler_colonial_reading, suppression_requirement, 1967, 0.76).
narrative_ontology:measurement(zlb_scr_su_t1977, zionist_legitimacy_basis__settler_colonial_reading, suppression_requirement, 1977, 0.73).
narrative_ontology:measurement(zlb_scr_su_t1987, zionist_legitimacy_basis__settler_colonial_reading, suppression_requirement, 1987, 0.81).
narrative_ontology:measurement(zlb_scr_su_t1993, zionist_legitimacy_basis__settler_colonial_reading, suppression_requirement, 1993, 0.66).
narrative_ontology:measurement(zlb_scr_su_t2000, zionist_legitimacy_basis__settler_colonial_reading, suppression_requirement, 2000, 0.77).
narrative_ontology:measurement(zlb_scr_su_t2015, zionist_legitimacy_basis__settler_colonial_reading, suppression_requirement, 2015, 0.84).
narrative_ontology:measurement(zlb_scr_su_t2025, zionist_legitimacy_basis__settler_colonial_reading, suppression_requirement, 2025, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(zionist_legitimacy_basis__settler_colonial_reading, resource_allocation).
narrative_ontology:affects_constraint(zionist_legitimacy_basis__settler_colonial_reading, zionist_legitimacy_basis__national_liberation_reading).
narrative_ontology:affects_constraint(zionist_legitimacy_basis__settler_colonial_reading, zionist_legitimacy_basis__religious_restoration_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the colloquial label 'Zionism'. The label conflates three structurally distinct legitimacy claims, decomposed per the epsilon-invariance principle into three stories over the same standing arrangement: this settler-colonial reading (displacement constitutive, maximal victim set, high epsilon), the national-liberation reading (persecuted-indigenous return, refugee absorption reframed as rescue, materially lower epsilon), and the religious-restoration reading (divine-promise grounding, post-1967 territorial claims sacralized). Each carries its own epsilon, beneficiaries, victims, and claimed type; they are linked here because the national-liberation account is the upstream mainstream narrative this reading argues against, and this reading's critique reshapes the operating environment of the religious-restoration reading after 1967.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
