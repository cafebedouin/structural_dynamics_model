% ============================================================================
% CONSTRAINT STORY: zionist_legitimacy_basis__national_liberation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_zionist_legitimacy_basis__national_liberation_reading, []).

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
 *   constraint_id: zionist_legitimacy_basis__national_liberation_reading
 *   human_readable: Zionist Legitimacy Basis - National-Liberation Reading
 *   domain: political_history/nationalism/settler_colonialism_studies
 *
 * SUMMARY:
 *   This story instantiates the national-liberation reading of the contested
 *   kernel 'basis of Zionist legitimacy'. On this reading, the standing
 *   arrangement - the movement's warrant (Basel 1897, Balfour 1917, the
 *   Mandate, Partition Resolution 181), the 1948 war and displacement, and
 *   the state's continuing administration - is the rescue and restoration of
 *   a persecuted people to its ancestral land; the displacement it caused is
 *   weighed as wartime necessity subordinated to liberation, and Arab
 *   objection is classified as refusal of Jewish rights rather than as a
 *   rival claim to be balanced. Epsilon is authored from that seat over the
 *   standing arrangement as it actually exists (not over any perfected
 *   version the reading would endorse): it concedes real and growing costs to
 *   Palestinians while holding them morally subordinate to the persecution it
 *   answers. The sibling readings - settler_colonial and
 *   religious_restoration - are separate constraint stories linked through
 *   network edges; they author different epsilon over the same referent,
 *   which is precisely the kernel contest. Claim and metrics are independent:
 *   the claimed type is tangled_rope (a genuine refuge and coordination
 *   function carrying asymmetric costs under active enforcement), and the
 *   metric series describe the frame's actual operation from this reading's
 *   own lights.
 *
 * KEY AGENTS:
 *   - - persecuted_european_jewry: primary beneficiary (powerless/trapped) - their testimony supplies the warrant; refuge flows to them through the arrangement
 *   - - mizrahi_expelled_communities: secondary beneficiary (moderate/constrained) - absorbed after expulsion from Arab lands; anchor the symmetry argument
 *   - - diaspora_jewish_communities: standing beneficiary (organized/mobile) - identity assurance and refuge optionality; fund and lobby
 *   - - zionist_movement_leadership: agenda-setter at founding (institutional/identity_locked) - wrote the warrant, fused movement identity to the territorial aim
 *   - - israeli_state_institutions: current agenda-setter (institutional/identity_locked) - administers the frame as constitutive self-description
 *   - - palestinian_displaced_population: primary target (organized/trapped) - bears the 1948 transfer across generations; return barred
 *   - - palestinians_under_israeli_rule: continuing target (moderate/constrained) - occupation, blockade, and differential treatment inside the frame's jurisdiction
 *   - - arab_neighbor_states: paying adversary (institutional/constrained) - war and absorption costs; partial normalization exit
 *   - - great_power_patrons: beneficiary with agenda-setting hand (institutional/mobile) - ratify, arm, shield; collect alignment rents
 *   - - palestinian_solidarity_movements: excluded voice (organized/mobile) - presses the counter-narrative outside the frame's legitimating arenas
 *   - - un_and_international_legal_bodies: analytical observer (institutional/analytical) - registers, resolves, adjudicates; feeds the resistance the frame absorbs
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(zionist_legitimacy_basis__national_liberation_reading, 0.56).
domain_priors:suppression_score(zionist_legitimacy_basis__national_liberation_reading, 0.74).
domain_priors:theater_ratio(zionist_legitimacy_basis__national_liberation_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(zionist_legitimacy_basis__national_liberation_reading, extractiveness, 0.56).
narrative_ontology:constraint_metric(zionist_legitimacy_basis__national_liberation_reading, suppression_requirement, 0.74).
narrative_ontology:constraint_metric(zionist_legitimacy_basis__national_liberation_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(zionist_legitimacy_basis__national_liberation_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(zionist_legitimacy_basis__national_liberation_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(zionist_legitimacy_basis__national_liberation_reading, tangled_rope).
narrative_ontology:human_readable(zionist_legitimacy_basis__national_liberation_reading, "Zionist Legitimacy Basis - National-Liberation Reading").
narrative_ontology:topic_domain(zionist_legitimacy_basis__national_liberation_reading, "political_history/nationalism/settler_colonialism_studies").

domain_priors:requires_active_enforcement(zionist_legitimacy_basis__national_liberation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(zionist_legitimacy_basis__national_liberation_reading, '1b623030-dfbe-4a7a-8cd0-e92ce819dca3').
narrative_ontology:cs_kernel_codification('1b623030-dfbe-4a7a-8cd0-e92ce819dca3', fixed_text).
narrative_ontology:cs_authority_grounding('1b623030-dfbe-4a7a-8cd0-e92ce819dca3', lineage).
narrative_ontology:cs_interpretation_layer_present('1b623030-dfbe-4a7a-8cd0-e92ce819dca3').
narrative_ontology:cs_reading_relation('1b623030-dfbe-4a7a-8cd0-e92ce819dca3', zionist_legitimacy_basis__settler_colonial_reading, coexists_with).
narrative_ontology:cs_reading_relation('1b623030-dfbe-4a7a-8cd0-e92ce819dca3', zionist_legitimacy_basis__religious_restoration_reading, influences).
narrative_ontology:cs_axiom('1b623030-dfbe-4a7a-8cd0-e92ce819dca3', foundational, persecution_necessity_licenses_return).
narrative_ontology:cs_axiom_status(persecution_necessity_licenses_return, holdable).
narrative_ontology:cs_axiom_grounding('1b623030-dfbe-4a7a-8cd0-e92ce819dca3', persecution_necessity_licenses_return, empirically_contingent).
narrative_ontology:cs_axiom('1b623030-dfbe-4a7a-8cd0-e92ce819dca3', foundational, ancestral_connection_confers_self_determination_priority).
narrative_ontology:cs_axiom_status(ancestral_connection_confers_self_determination_priority, holdable).
narrative_ontology:cs_axiom_grounding('1b623030-dfbe-4a7a-8cd0-e92ce819dca3', ancestral_connection_confers_self_determination_priority, deontological).
narrative_ontology:cs_axiom('1b623030-dfbe-4a7a-8cd0-e92ce819dca3', secondary, arab_objection_coded_as_rights_denial_not_counterclaim).
narrative_ontology:cs_axiom_status(arab_objection_coded_as_rights_denial_not_counterclaim, holdable).
narrative_ontology:cs_axiom_grounding('1b623030-dfbe-4a7a-8cd0-e92ce819dca3', arab_objection_coded_as_rights_denial_not_counterclaim, conventional).
narrative_ontology:cs_reference_frame('1b623030-dfbe-4a7a-8cd0-e92ce819dca3', indigenous_return_self_determination).
narrative_ontology:cs_drift_state('1b623030-dfbe-4a7a-8cd0-e92ce819dca3', contemporary_human_rights_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('1b623030-dfbe-4a7a-8cd0-e92ce819dca3', '').
narrative_ontology:cs_kernel_id(zionist_legitimacy_basis__national_liberation_reading, zionist_legitimacy_basis).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(zionist_legitimacy_basis__national_liberation_reading, persecuted_european_jewry).
narrative_ontology:constraint_beneficiary(zionist_legitimacy_basis__national_liberation_reading, mizrahi_expelled_communities).
narrative_ontology:constraint_beneficiary(zionist_legitimacy_basis__national_liberation_reading, diaspora_jewish_communities).
narrative_ontology:constraint_beneficiary(zionist_legitimacy_basis__national_liberation_reading, great_power_patrons).
narrative_ontology:constraint_victim(zionist_legitimacy_basis__national_liberation_reading, palestinian_displaced_population).
narrative_ontology:constraint_victim(zionist_legitimacy_basis__national_liberation_reading, palestinians_under_israeli_rule).
narrative_ontology:constraint_victim(zionist_legitimacy_basis__national_liberation_reading, arab_neighbor_states).
narrative_ontology:constraint_vindicates(zionist_legitimacy_basis__national_liberation_reading, self_determination_doctrine).
narrative_ontology:constraint_vindicates(zionist_legitimacy_basis__national_liberation_reading, balfour_declaration_warrant).
narrative_ontology:constraint_vindicates(zionist_legitimacy_basis__national_liberation_reading, un_resolution_181_partition_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Jewish communities of Europe from the 1880s onward faced intensifying legal exclusion, pogroms, and finally attempted annihilation. The national project took their testimony as its moral warrant and, after 1948, provided the refuge no other door opened - the Evian conference of 1938 closed more doors than it opened. Their safety thereafter flows through the state the movement built; stepping outside that frame would mean entrusting safety again to hosts whose record produced the project in the first place.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__national_liberation_reading, persecuted_european_jewry, beneficiary,
    powerless, biographical, trapped, continental).

% Convened the national movement at Basel in 1897, secured the Balfour Declaration and the Mandate terms, directed immigration, land purchase, and self-defense, and accepted the partition boundary in 1947. Sets the terms under which persecution testimony and ancestral connection ground political claims; the movement's identity fused with the territorial aim early, and the alternative-territory offer raised at the time was rejected outright.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__national_liberation_reading, zionist_movement_leadership, agenda_setter,
    institutional, generational, identity_locked, global).

% Administers the state whose founding account this frame supplies: the Law of Return, the bar on refugee return, settlement administration in the territories held since 1967, and the official school-and-ceremony historiography. The frame is the state's self-description; revising it would unsettle the state's own legitimacy narrative, so the institutions maintain it as constitutive rather than as one option among others.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__national_liberation_reading, israeli_state_institutions, agenda_setter,
    institutional, generational, identity_locked, national).

% Roughly 700,000 Arabs left or were driven from towns and villages in the 1948 war; registration passed to children and grandchildren, now numbering in the millions across camps in Lebanon, Syria, Jordan, and the territories. Return is barred under the frame that codes the return demand as hostility to the national claim. Organized politics descended from the PLO and camp committees carries little leverage over the arrangement; individual exit means surrendering the registered claim.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__national_liberation_reading, palestinian_displaced_population, payer,
    organized, generational, trapped, regional).

% Arab citizens inside the 1949 lines hold formal citizenship amid documented gaps in budgets, planning approval, and recognition; West Bank residents have lived under occupation and expanding settlements for over five decades; Gaza's residents live under blockade and recurrent war. Within the frame, their objections are received as expressions of hostility to the national project rather than as claims to be weighed on their own terms.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__national_liberation_reading, palestinians_under_israeli_rule, payer,
    moderate, biographical, constrained, regional).

% Fought the 1948 and subsequent wars against the new state, absorbed refugee populations (with Lebanon and Syria withholding citizenship), enforced boycotts, and paid heavily in lives and treasury. Egypt and Jordan later signed treaties, and several Gulf states normalized relations in 2020 - converting opposition into managed coexistence while leaving the displaced-population file unresolved. Full reconciliation remains costly and domestically fraught.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__national_liberation_reading, arab_neighbor_states, payer,
    institutional, generational, constrained, regional).

% Britain issued the Balfour Declaration and held the Mandate; the United States recognized the state minutes after its declaration, supplies multi-billion-dollar annual military aid, and shields it diplomatically at the Security Council. They receive strategic intelligence cooperation, regional anchoring, and domestic coalition value; support can be recalibrated, but at measurable strategic and political cost.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__national_liberation_reading, great_power_patrons, beneficiary,
    institutional, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(zionist_legitimacy_basis__national_liberation_reading, great_power_patrons, agenda_setter).

% Draw identity assurance and a standing refuge option from the arrangement; remit donations, exercise lobbying, and hold the immigration option. Individual members can and do disengage - assimilation, third-country emigration - without forfeiting communal membership elsewhere, so the tie is strong but not compulsory.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__national_liberation_reading, diaspora_jewish_communities, beneficiary,
    organized, generational, mobile, global).

% Jews of Arab and Muslim lands, numbering roughly 850,000 mid-century, exited or were expelled from Morocco to Iraq between 1948 and the 1970s, losing property, and were absorbed into Israel - becoming the demographic core of the population-exchange symmetry argument the frame uses. Their integration, cultural revival, and property-loss claims are bound up with the state the frame sustains.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__national_liberation_reading, mizrahi_expelled_communities, beneficiary,
    moderate, biographical, constrained, regional).

% Boycott-divestment networks, campus organizing, and academic decolonial scholarship press the counter-narrative internationally. Inside the frame's own arenas - legislatures, federations, patron-state media - their claims tend to be received as prejudicial denial rather than as argument, so their voice circulates mainly outside the legitimating conversation.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__national_liberation_reading, palestinian_solidarity_movements, excluded,
    organized, generational, mobile, global).

% Register and serve the refugee population through a dedicated agency since 1949, pass recurring General Assembly resolutions, and adjudicate - barrier advisory opinions, treaty-body reviews, court proceedings and inquiries. They document and assess; their findings feed the international pressure the frame absorbs and contests.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__national_liberation_reading, un_and_international_legal_bodies, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(zionist_legitimacy_basis__national_liberation_reading, israeli_state_institutions).
narrative_ontology:fixing_cost_class(zionist_legitimacy_basis__national_liberation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem of a stateless, scattered minority facing coordinated persecution: pools immigration, capital, diplomacy, and defense into one national project with a fixed territorial address, giving dispersed communities a common refuge commitment, legal personality, and negotiating existence (Mandate, then state).
% TRANSFER_FUNCTION: Moves land, housing, and sovereign space in Mandatory Palestine - territory beyond the 1949 lines after the 1948 war, plus the areas held after 1967 - from the Arab population to the incoming Jewish national collective; moves diaspora money, migrants, and lobbying inward; moves patron-state arms, aid, and diplomatic cover toward the state.
% ABSENT_VOICES: The Arab population of the mandated territory - the people who would lose the land - held no seat where the warrant was written: not at Basel, not in the corridor that produced the Balfour Declaration, not at San Remo; the 1947 partition recommendation passed over their objection. Their descendants remain outside the frame's legitimating conversations, entering chiefly as objection to be absorbed. Also absent at the founding: the Middle Eastern Jewish communities later folded into the symmetry argument, consulted only after their expulsion.
% DISAPPEARANCE_RATIONALE: If the frame vanished overnight, the state loses its founding account - the immigration law's warrant, the return bar's justification, the patron relationship's premise; the diaspora's refuge commitment and identity assurance lapse; the refugee file reopens on radically different terms; and the regional alignment architecture of treaties, normalization agreements, and aid streams goes to renegotiation. Nothing about the arrangement is self-sustaining without the frame that grounds it.
% FOUNDING_PROBLEM: European Jewry's condition from the 1880s: legal exclusion, pogroms, and ultimately attempted annihilation, with no sovereign capacity and no open refuge - demonstrated negatively by the Evian conference's failures and the 1939 restriction white paper.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: the historical record of persecution is established by Nuremberg documentation, contemporaneous diplomatic files, and Holocaust historiography; ongoing antisemitism is tracked by intergovernmental monitoring bodies, fundamental-rights agencies, and police hate-crime statistics in states with no stake in the frame. What no external party attests is that this particular arrangement remains the necessary remedy - that inference belongs to the frame's own holders, so the corroboration covers the problem, not the prescription.
narrative_ontology:disappearance_verdict(zionist_legitimacy_basis__national_liberation_reading, world_rearranges).
narrative_ontology:founding_problem_status(zionist_legitimacy_basis__national_liberation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(zionist_legitimacy_basis__national_liberation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(zionist_legitimacy_basis__national_liberation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(zionist_legitimacy_basis__national_liberation_reading, 0.56, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(zionist_legitimacy_basis__national_liberation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(zionist_legitimacy_basis__national_liberation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(zionist_legitimacy_basis__national_liberation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness ends at 0.56 from this seat: the frame concedes the displacement's magnitude and the occupation's continuation while attributing causation to rejectionist war and holding the costs subordinate to the persecution answered - far below what a challenger reading would author, well above zero because the displaced population is real and the burden compounds across generations. Suppression (0.74) tracks an enforcement ratchet: anti-boycott statutes, definitional controversies over antisemitism criteria, memory restrictions in Israeli law, donor pressure on universities, and a permanent information-war apparatus. Theater (0.40) rises as commemoration, advocacy ritual, and anniversary machinery claim a growing share of activity beside the still-functional refuge channel. All three series run on one shared eight-point grid (1897-2025) with every metric authored at every point, so drift detection samples a complete matrix and no end-state scalar substitutes into earlier rows. Accessibility collapse is moderate (0.58): inside the frame, once persecution-necessity and ancestral priority are granted, binational and return-centered alternatives lose their footing; outside it, rivals persist and grow. Resistance is high (0.80): armed, diplomatic, legal, and scholarly contestation is the frame's permanent operating environment.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the agenda-setting seats compute differently by construction. From israeli_state_institutions and the beneficiary seats, the arrangement is a lifeline that worked - persecution answered, refuge delivered, identity sustained; costs register as regrettable, externally caused, and morally diminishing next to the alternative of no refuge. From palestinian_displaced_population and palestinians_under_israeli_rule, the same structure is the machine that took the towns, bars the return, and codes every objection as hostility - an experience shaped like enforced extraction with no exit across generations. The engine computes this divergence from power, exit, and declared role; nothing in the authored claim reconciles it. Great_power_patrons sit between: beneficiaries of alignment, intermittent critics, structurally insulated by mobility.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations track real flows: persecuted_european_jewry and mizrahi_expelled_communities received refuge and incorporation (near the beneficiary pole); diaspora_jewish_communities collect identity assurance and optionality; great_power_patrons collect alignment and intelligence rents. Victim declarations track likewise: palestinian_displaced_population bears the transfer with trapped, heritable exit (near the full-target pole, amplified by the absence of arbitrage - neither geography nor registration status opens a door); palestinians_under_israeli_rule bear ongoing occupation costs under constrained exit; arab_neighbor_states bore war and absorption costs, with normalization providing partial, expensive exit that moderates their position somewhat. No directionality overrides are declared: no two agents share a power atom while occupying opposed structural relations in a way the derivation cannot distinguish, since within the institutional set role and exit options already differentiate the seats.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem - stateless, persecutable Jews with no open refuge - remains live: antisemitism is monitored upward across OSCE states by agencies with no stake in this frame, and recent history (the post-Soviet exodus, the post-2023 surge) renewed the refuge function. Declaring the mandate resolved would be false. The tangled-rope classification is what blocks the two standard mislabels: reading the frame as pure coordination-erasure of cost would erase the displaced population's standing claim; reading it as pure extraction-cover would erase the refuge function that materially saved lives and still operates. Holding both halves visible - genuine coordination, asymmetric payment, active enforcement - is the analytical point of authoring this seat honestly rather than defensively.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sibling_settler_colonial_delta,
    'This story instantiates the national_liberation_reading of kernel zionist_legitimacy_basis; what changes structurally if the settler_colonial_reading is adopted instead?',
    'Read alongside the sibling constraint story: compare its epsilon, victim declarations, and enforcement reading over the same referent. Adoption would reclassify Arab opposition from rights-denial to anti-colonial resistance and raise authored epsilon sharply.',
    'Classification shifts toward pure extraction with higher effective extraction on the state seats; gain_flow and fixing_cost stay unchanged; the legitimacy apparatus itself becomes the contested object rather than the liberation instrument.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_settler_colonial_delta, conceptual, 'Committer delta versus the settler-colonial sibling reading.').

omega_variable(
    sibling_religious_delta,
    'What changes structurally if the religious_restoration_reading of the same kernel is adopted - divine promise and messianic process rather than persecution-necessity and ancestral priority?',
    'Sibling-story comparison: trace where grounding shifts from empirically contingent persecution claims to theological warrant, and what that does to falsifiability and foreclosure routing.',
    'Temporal horizon extends to civilizational; displacement becomes irrevocable duty rather than a weighed cost; cost-to-fix reasoning inverts (prohibitive by sanctity rather than by entrenchment), and empirical refutation stops touching the foundational axiom.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_religious_delta, conceptual, 'Committer delta versus the religious-restoration sibling reading.').

omega_variable(
    refuge_alternative_availability,
    'Were non-territorial or alternative-territorial refuges genuinely unavailable when the necessity-license was invoked (Evian 1938, US and British quota regimes, the rejected Uganda offer)?',
    'Diplomatic-archive reconstruction of doors closed per year against doors available, plus counterfactual absorption-capacity estimates for receiving states.',
    'If adequate refuge existed, persecution-necessity licenses less and the frame''s warrant erodes; if doors were genuinely shut, the license strengthens and epsilon from this seat drops.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(refuge_alternative_availability, empirical, 'Whether the necessity half of the frame''s warrant was empirically forced.').

omega_variable(
    flight_causation_historiography,
    'Were the 1948 departures expulsion, panic-flight, or Arab-command evacuation - in what proportions, locality by locality?',
    'Opened IDF and Arab military archives, village-level reconstruction, and the New Historians'' document base against traditional-national accounts.',
    'This reading''s moral discount of the displacement leans on command-evacuation and voluntary-flight shares; archival confirmation of systematic expulsion would force epsilon upward and strain the liberation classification toward the sibling''s.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(flight_causation_historiography, empirical, 'Load-bearing causal historiography beneath the frame''s discount of displacement.').

omega_variable(
    indigenity_status_framing,
    'Is indigenous return descriptively apt for a population substantially diasporic for eighteen centuries with partial continuous presence, or is it aspirational identity-framing doing justificatory work?',
    'Neutral application of comparative indigenity criteria (continuity, self-identification, prior occupancy) to both populations by scholarship outside the frame''s institutions.',
    'If the label is aspirational, the restoration premise weakens, the frame defends borrowed rather than inherited authority, and theater rises while accessibility collapse falls.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(indigenity_status_framing, conceptual, 'Descriptive adequacy of the indigenous-return premise.').

omega_variable(
    suppression_structural_vs_internalized,
    'How much of the measured suppression is external enforcement (statutes, platform pressure, funding leverage) versus internalized communal policing (dissent priced as betrayal within families, congregations, federations)?',
    'Interview and survey evidence on in-community dissent costs, cross-checked against legal and platform-incidence data on external enforcement.',
    'A large internalized share means suppression travels with agents after they leave enforcing jurisdictions - effective suppression runs above what statute counts show, and legal remedies alone will not release it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Structural versus internalized composition of the frame''s enforcement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(zionist_legitimacy_basis__national_liberation_reading, 1897, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(zion_natlib_tr_t1897, zionist_legitimacy_basis__national_liberation_reading, theater_ratio, 1897, 0.05).
narrative_ontology:measurement(zion_natlib_tr_t1917, zionist_legitimacy_basis__national_liberation_reading, theater_ratio, 1917, 0.08).
narrative_ontology:measurement(zion_natlib_tr_t1939, zionist_legitimacy_basis__national_liberation_reading, theater_ratio, 1939, 0.12).
narrative_ontology:measurement(zion_natlib_tr_t1948, zionist_legitimacy_basis__national_liberation_reading, theater_ratio, 1948, 0.18).
narrative_ontology:measurement(zion_natlib_tr_t1967, zionist_legitimacy_basis__national_liberation_reading, theater_ratio, 1967, 0.2).
narrative_ontology:measurement(zion_natlib_tr_t1988, zionist_legitimacy_basis__national_liberation_reading, theater_ratio, 1988, 0.26).
narrative_ontology:measurement(zion_natlib_tr_t2000, zionist_legitimacy_basis__national_liberation_reading, theater_ratio, 2000, 0.32).
narrative_ontology:measurement(zion_natlib_tr_t2025, zionist_legitimacy_basis__national_liberation_reading, theater_ratio, 2025, 0.4).

% Extraction over time
narrative_ontology:measurement(zion_natlib_be_t1897, zionist_legitimacy_basis__national_liberation_reading, base_extractiveness, 1897, 0.15).
narrative_ontology:measurement(zion_natlib_be_t1917, zionist_legitimacy_basis__national_liberation_reading, base_extractiveness, 1917, 0.25).
narrative_ontology:measurement(zion_natlib_be_t1939, zionist_legitimacy_basis__national_liberation_reading, base_extractiveness, 1939, 0.35).
narrative_ontology:measurement(zion_natlib_be_t1948, zionist_legitimacy_basis__national_liberation_reading, base_extractiveness, 1948, 0.45).
narrative_ontology:measurement(zion_natlib_be_t1967, zionist_legitimacy_basis__national_liberation_reading, base_extractiveness, 1967, 0.5).
narrative_ontology:measurement(zion_natlib_be_t1988, zionist_legitimacy_basis__national_liberation_reading, base_extractiveness, 1988, 0.52).
narrative_ontology:measurement(zion_natlib_be_t2000, zionist_legitimacy_basis__national_liberation_reading, base_extractiveness, 2000, 0.54).
narrative_ontology:measurement(zion_natlib_be_t2025, zionist_legitimacy_basis__national_liberation_reading, base_extractiveness, 2025, 0.56).

% Suppression requirement over time
narrative_ontology:measurement(zion_natlib_su_t1897, zionist_legitimacy_basis__national_liberation_reading, suppression_requirement, 1897, 0.1).
narrative_ontology:measurement(zion_natlib_su_t1917, zionist_legitimacy_basis__national_liberation_reading, suppression_requirement, 1917, 0.2).
narrative_ontology:measurement(zion_natlib_su_t1939, zionist_legitimacy_basis__national_liberation_reading, suppression_requirement, 1939, 0.35).
narrative_ontology:measurement(zion_natlib_su_t1948, zionist_legitimacy_basis__national_liberation_reading, suppression_requirement, 1948, 0.55).
narrative_ontology:measurement(zion_natlib_su_t1967, zionist_legitimacy_basis__national_liberation_reading, suppression_requirement, 1967, 0.6).
narrative_ontology:measurement(zion_natlib_su_t1988, zionist_legitimacy_basis__national_liberation_reading, suppression_requirement, 1988, 0.62).
narrative_ontology:measurement(zion_natlib_su_t2000, zionist_legitimacy_basis__national_liberation_reading, suppression_requirement, 2000, 0.68).
narrative_ontology:measurement(zion_natlib_su_t2025, zionist_legitimacy_basis__national_liberation_reading, suppression_requirement, 2025, 0.74).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(zionist_legitimacy_basis__national_liberation_reading, identity_coordination).
narrative_ontology:affects_constraint(zionist_legitimacy_basis__national_liberation_reading, zionist_legitimacy_basis__settler_colonial_reading).
narrative_ontology:affects_constraint(zionist_legitimacy_basis__national_liberation_reading, zionist_legitimacy_basis__religious_restoration_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the natural-language concept 'Zionism's legitimacy' decomposes into three structurally distinct readings of one kernel, each with its own stable epsilon over the same referent. This national-liberation reading authors epsilon at 0.56 (displacement conceded but morally subordinated to persecution answered); the settler-colonial sibling authors sharply higher epsilon over identical events; the religious-restoration sibling relocates the warrant from empirical persecution claims to theological promise. The national-liberation reading is upstream in institutional terms - its diplomatic victories (Balfour, Mandate, partition, recognition) created the state whose assets both siblings contest - so its edges run to both dependents. Per epsilon-invariance, no single story averages across readings; the contest lives in the family structure, not inside any one file.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
