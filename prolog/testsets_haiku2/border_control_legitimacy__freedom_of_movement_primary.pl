% ============================================================================
% CONSTRAINT STORY: border_control_legitimacy__freedom_of_movement_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_border_control_legitimacy__freedom_of_movement_primary, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: border_control_legitimacy__freedom_of_movement_primary
 *   human_readable: Border Closure Authority Under Freedom of Movement Reading
 *   domain: political_philosophy/international_law
 *
 * SUMMARY:
 *   This constraint story instantiates the FREEDOM_OF_MOVEMENT_PRIMARY
 *   reading of the border-control-legitimacy kernel. Under this reading,
 *   freedom of movement is a foundational human right that transcends state
 *   boundaries; territorial sovereignty is reinterpreted as jurisdictional
 *   authority (the right to regulate rights and obligations of those within
 *   territory) rather than as border closure authority. The constraint
 *   examined here is the border-closure arrangement itself, assessed through
 *   this reading's lights: it is a snare whose persistence depends on
 *   coercive enforcement that suppresses the mobility claims of displaced and
 *   economically migrating persons. The reading asks: if personhood rather
 *   than citizenship is the primary category, what legitimates exclusion?
 *   Answer: under this reading, nothing. Border closure is thus structurally
 *   extractive — it concentrates opportunity in the hands of citizens while
 *   extracting from non-citizens who have nowhere else to go. The constraint
 *   is CLAIMED as snare and the metrics reflect substantial extraction and
 *   active enforcement, so there is no claim/metric divergence here.
 *
 * KEY AGENTS:
 *   - Restrictive state apparatus: institutional power, arbitrage exit — sets and enforces closure rules; collects political and enforcement legitimacy
 *   - Displaced persons: powerless, trapped — bear the full cost of exclusion; no meaningful exit
 *   - Economic migrants: moderate power, constrained exit — pay through smuggling, underground labor, or poverty
 *   - Asylum seekers: powerless, identity-locked — bound to their status as asylum claimants; cannot unbecome the category the state rejects
 *   - Nationalist political movements: powerful, mobile — benefit from closure authority and restrictive messaging; invest in enforcement
 *   - Transnational human-rights bodies: institutional, analytical — document the constraint's illegitimacy but cannot dismantle it
 *   - Labor-scarce employers: excluded but powerful — would benefit from mobility but are silenced by security framing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(border_control_legitimacy__freedom_of_movement_primary, 0.82).
domain_priors:suppression_score(border_control_legitimacy__freedom_of_movement_primary, 0.76).
domain_priors:theater_ratio(border_control_legitimacy__freedom_of_movement_primary, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(border_control_legitimacy__freedom_of_movement_primary, extractiveness, 0.82).
narrative_ontology:constraint_metric(border_control_legitimacy__freedom_of_movement_primary, suppression_requirement, 0.76).
narrative_ontology:constraint_metric(border_control_legitimacy__freedom_of_movement_primary, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(border_control_legitimacy__freedom_of_movement_primary, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(border_control_legitimacy__freedom_of_movement_primary, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(border_control_legitimacy__freedom_of_movement_primary, snare).
narrative_ontology:human_readable(border_control_legitimacy__freedom_of_movement_primary, "Border Closure Authority Under Freedom of Movement Reading").
narrative_ontology:topic_domain(border_control_legitimacy__freedom_of_movement_primary, "political_philosophy/international_law").

domain_priors:requires_active_enforcement(border_control_legitimacy__freedom_of_movement_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(border_control_legitimacy__freedom_of_movement_primary, '7d9a3b77-954b-4a09-8bb2-fa63b8bc04ce').
narrative_ontology:cs_kernel_codification('7d9a3b77-954b-4a09-8bb2-fa63b8bc04ce', formalized).
narrative_ontology:cs_authority_grounding('7d9a3b77-954b-4a09-8bb2-fa63b8bc04ce', extraction).
narrative_ontology:cs_interpretation_layer_present('7d9a3b77-954b-4a09-8bb2-fa63b8bc04ce').
narrative_ontology:cs_reading_relation('7d9a3b77-954b-4a09-8bb2-fa63b8bc04ce', border_control_legitimacy__sovereignty_primary, forecloses).
narrative_ontology:cs_reading_relation('7d9a3b77-954b-4a09-8bb2-fa63b8bc04ce', border_control_legitimacy__jurisdictional_sovereignty, influences).
narrative_ontology:cs_axiom('7d9a3b77-954b-4a09-8bb2-fa63b8bc04ce', foundational, personhood_primary_identity).
narrative_ontology:cs_axiom_status(personhood_primary_identity, holdable).
narrative_ontology:cs_axiom_grounding('7d9a3b77-954b-4a09-8bb2-fa63b8bc04ce', personhood_primary_identity, deontological).
narrative_ontology:cs_axiom('7d9a3b77-954b-4a09-8bb2-fa63b8bc04ce', foundational, freedom_of_movement_universal_right).
narrative_ontology:cs_axiom_status(freedom_of_movement_universal_right, holdable).
narrative_ontology:cs_axiom_grounding('7d9a3b77-954b-4a09-8bb2-fa63b8bc04ce', freedom_of_movement_universal_right, deontological).
narrative_ontology:cs_axiom('7d9a3b77-954b-4a09-8bb2-fa63b8bc04ce', secondary, citizenship_secondary_to_personhood).
narrative_ontology:cs_axiom_status(citizenship_secondary_to_personhood, holdable).
narrative_ontology:cs_axiom_grounding('7d9a3b77-954b-4a09-8bb2-fa63b8bc04ce', citizenship_secondary_to_personhood, deontological).
narrative_ontology:cs_reference_frame('7d9a3b77-954b-4a09-8bb2-fa63b8bc04ce', freedom_of_movement_as_foundational_right).
narrative_ontology:cs_drift_state('7d9a3b77-954b-4a09-8bb2-fa63b8bc04ce', contemporary_nationalist_resurgence, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('7d9a3b77-954b-4a09-8bb2-fa63b8bc04ce', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(border_control_legitimacy__freedom_of_movement_primary, border_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(border_control_legitimacy__freedom_of_movement_primary, restrictive_state_apparatus).
narrative_ontology:constraint_beneficiary(border_control_legitimacy__freedom_of_movement_primary, nationalist_political_movements).
narrative_ontology:constraint_victim(border_control_legitimacy__freedom_of_movement_primary, displaced_persons).
narrative_ontology:constraint_victim(border_control_legitimacy__freedom_of_movement_primary, economic_migrants).
narrative_ontology:constraint_victim(border_control_legitimacy__freedom_of_movement_primary, asylum_seekers).
narrative_ontology:constraint_victim(border_control_legitimacy__freedom_of_movement_primary, workers_denied_mobility).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(border_control_legitimacy__freedom_of_movement_primary, workers_denied_mobility).
narrative_ontology:constraint_beneficiary(border_control_legitimacy__freedom_of_movement_primary, receiving_country_publics).
narrative_ontology:constraint_victim(border_control_legitimacy__freedom_of_movement_primary, receiving_country_publics).
narrative_ontology:constraint_vindicates(border_control_legitimacy__freedom_of_movement_primary, freedom_of_movement_as_human_right).
narrative_ontology:constraint_vindicates(border_control_legitimacy__freedom_of_movement_primary, universal_personhood_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% State administration that sets and enforces border closure policy. Claims legitimacy through sovereign prerogative and public security rationale. Maintains enforcement infrastructure (border personnel, surveillance, detention facilities) and controls the rules governing entry. Collects political capital from restrictive enforcement and avoids accountability for harms to excluded persons.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__freedom_of_movement_primary, restrictive_state_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).

% Persons fleeing violence, persecution, or environmental collapse who have no safe alternative location. Bears the full cost of exclusion: separation from family, denial of livelihood, exposure to violence, and loss of legal personhood. Cannot exit the constraint meaningfully — returning to origin means death or persecution; alternative territories are equally closed; remaining in liminal space (camps, smuggling routes) is the only option.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__freedom_of_movement_primary, displaced_persons, payer,
    powerless, immediate, trapped, universal).

% Workers seeking better employment and living standards in labor-scarce jurisdictions. The constraint forces them either to pay smugglers (transferring wealth to criminal networks), to accept exploitative wages in underground economies, or to remain in poverty in origin countries. Exit options exist theoretically (legal immigration pathways) but are functionally closed by caps, quotas, and skill filters that exclude the majority.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__freedom_of_movement_primary, economic_migrants, payer,
    moderate, biographical, constrained, regional).

% Persons claiming protection from persecution or serious harm, bound by legal identity to asylum-defined status. The constraint denies them entry and reclassifies their claim as 'economic migration' or 'security threat.' They are identity-locked: their status as asylum claimants exists only if recognized by the state whose borders they seek to cross; rejection strips them of even the claim to protection. They cannot unbecome asylum seekers by exiting the frame.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__freedom_of_movement_primary, asylum_seekers, payer,
    powerless, immediate, identity_locked, universal).

% Domestic labor force that benefits from reduced job competition and higher wages (secondary beneficiary role) while also paying higher costs for goods/services that depend on cross-border labor (dual cost). The constraint extracts from them indirectly by inflating consumer prices while extracting politically through nationalist messaging that pins economic anxiety on external migrants rather than on policy choices.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__freedom_of_movement_primary, workers_denied_mobility, payer,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(border_control_legitimacy__freedom_of_movement_primary, workers_denied_mobility, beneficiary).

% International courts, UN bodies, and treaty-monitoring mechanisms tasked with adjudicating freedom of movement claims. They produce legal opinions (ICJ rulings, treaty body general comments) that declare border closure illegitimate under freedom of movement frameworks, but lack enforcement power to compel state compliance. Their role is to document the constraint and its harms; they cannot dismantle it.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__freedom_of_movement_primary, transnational_human_rights_bodies, observer,
    institutional, generational, analytical, universal).

% Political forces that benefit from border closure authority and the nationalist framing of personhood (national citizenship as the primary identity, non-citizens as external threats). The constraint delivers electoral advantage through restriction messaging and rallying support around border enforcement. They invest in maintaining the constraint because it generates political power.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__freedom_of_movement_primary, nationalist_political_movements, beneficiary,
    powerful, biographical, mobile, national).

% Employers in sectors facing skill shortages (agriculture, healthcare, construction, tech) who would hire cross-border workers at lower regulatory cost. The constraint excludes them from the conversation about labor mobility policy; they have preferences (open labor markets) but are systematically silenced by nationalist rhetoric that treats labor restriction as security.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__freedom_of_movement_primary, labor_scarce_employers, excluded,
    powerful, biographical, constrained, national).

% General population of destination countries. Benefits from the symbolic security that border closure provides and from political messaging that attributes economic anxiety to external migrants (scapegoating mechanism). Also pays through reduced labor supply, higher prices for labor-intensive goods/services, and social costs of enforcement apparatus (policing, detention, deportation). The constraint extracts from them by converting labor scarcity into a security narrative rather than a policy problem.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__freedom_of_movement_primary, receiving_country_publics, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(border_control_legitimacy__freedom_of_movement_primary, receiving_country_publics, payer).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(border_control_legitimacy__freedom_of_movement_primary, restrictive_state_apparatus).
narrative_ontology:fixing_cost_class(border_control_legitimacy__freedom_of_movement_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Border closure apparatus coordinates a narrative of national security and sovereignty: it organizes the distribution of rights (who can access territory, work, social goods) around the category of citizenship rather than personhood. It 'solves' the coordination problem of 'how to allocate belonging' by fixing the category as the bounded nation-state rather than universal human status.
% TRANSFER_FUNCTION: Moves opportunity (access to labor markets, legal work, family reunion, safety) from non-citizens to citizens; moves political legitimacy and enforcement power from international human-rights bodies to national states; moves wealth from excluded persons (who pay smugglers or accept underground wages) to smuggling networks and to the state enforcement apparatus. Under the freedom-of-movement reading, these transfers are illegitimate because the underlying category (citizen vs. non-citizen as a moral boundary) is contested as incompatible with human rights.
% ABSENT_VOICES: Economic migrants rejected as 'merely economic' and excluded from asylum frameworks have no seat in formal policy debates about labor mobility; their presence is acknowledged only as a 'problem to solve' (trafficking, exploitation) rather than as a legitimate claim. Smuggling networks and underground employers are structurally absent but materially constitute the constraint's operation — their existence testifies to the constraint's coercive force, yet they are not named as parties because they are treated as pathologies rather than rational responses to closure.
% DISAPPEARANCE_RATIONALE: If border closure authority evaporated overnight, the global allocation of persons and opportunity would reorganize: migration patterns would shift from costly smuggling and underground labor to legal pathways; labor-scarce sectors would stabilize through cross-border recruitment; displaced persons would exit camps and liminal spaces; political coalitions would realign around labor-policy questions rather than security-nationalist framing. The constraint is not a structural inevitability of global order — it is a choice made by powerful states, and its disappearance would unmake that choice visibly.
% FOUNDING_PROBLEM: The founding problem (historically and in the doctrine) was administrative: how to organize state capacity to monitor and control entry for the purpose of tracking and taxing residents, conscripting soldiers, and enforcing public-health quarantines. The legitimacy claim shifted over time from administration (tracking residents) to sovereignty (absolute discretion) to security (protecting citizens from external threat).
% FOUNDING_PROBLEM_CORROBORATION: Historians of the state (James C. Scott, Charles Tilly, Aristide Zolberg) document that border control originated as a bureaucratic problem, not a security or sovereignty principle. The state's foundational administrative needs (census, conscription, taxation) have been substantially solved by digital identity systems, and those systems are decoupled from border closure — many states track residents without excluding all non-citizens. No state actor now credibly claims that border closure is necessary for taxation or conscription. The contemporary justification has shifted entirely to security and sovereignty, which are post-hoc rationalizations documented in policy studies, academic critiques, and legal depositions. The founding problem is dead; the apparatus persists through inertia and political capture.
narrative_ontology:disappearance_verdict(border_control_legitimacy__freedom_of_movement_primary, world_rearranges).
narrative_ontology:founding_problem_status(border_control_legitimacy__freedom_of_movement_primary, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(border_control_legitimacy__freedom_of_movement_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(border_control_legitimacy__freedom_of_movement_primary, 'none', 1).
narrative_ontology:epsilon_provenance(border_control_legitimacy__freedom_of_movement_primary, 0.82, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(border_control_legitimacy__freedom_of_movement_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(border_control_legitimacy__freedom_of_movement_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(border_control_legitimacy__freedom_of_movement_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.82) because the constraint concentrates opportunity (territory, work, legal status) in the hands of a bounded citizenry while systematically denying it to non-citizens, and because that denial is sustained by coercive force rather than by preference or natural advantage. The constraint does not offer genuine alternatives to those it excludes — they cannot simply 'leave' the constraint (they have nowhere else to go), and the boundaries that define the constraint shift as they move (every territorial state claims similar closure authority). Suppression is high (0.76) because enforcement requires active machinery (border patrol, detention, deportation, visa systems) whose cost and extent would collapse if not continuously renewed. Theater is moderate (0.48): some genuine security and administrative function exists (tracking disease, identifying criminal actors), but a substantial share of enforcement effort defends the nationalist narrative and political advantage rather than those functions. The measurement trajectory shows extraction rising early (as restrictive rhetoric hardens globally) and then plateauing (at a high equilibrium) — a pattern consistent with rent-seeking that reaches saturation when political capture is complete.
 *
 * PERSPECTIVAL GAP:
 *   The state-apparatus seat (agenda-setter, institutional power) experiences this constraint as sovereignty — a right, a prerogative, a marker of statehood. From that seat, the constraint is protective coordination (we organize how to distribute rights to OUR people). The displaced-person seat (powerless, trapped) experiences the constraint as coerced exclusion — a barrier with no legitimate justification. From that seat, the constraint is pure extraction (my options are zero, my costs are maximal). The engine computes these divergences from the authored structural data: power asymmetry, exit-option asymmetry, victim/beneficiary split. Neither seat is wrong; both are describing the same constraint from incommensurable positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Restrictive state apparatus: d = 0.05 (full beneficiary). Collects enforcement legitimacy, political power, and control over opportunity distribution. Has arbitrage-grade exit (can change policy, can federate with other states). The direction of the constraint runs from them to others. Displaced persons: d = 0.98 (full target). Bears the maximal cost, has no exit, no alternatives. Identity-locked asylum seekers: d = 0.95 (near-full target). Cannot exit by becoming non-asylum-seekers; the constraint follows them across borders. Economic migrants: d = 0.85 (strong target). Moderate power and some constrained exit options (smuggling, underground labor, visa pathways, though narrow), but fundamentally constrained by the apparatus. Nationalist movements: d = 0.12 (beneficiary). They benefit from the closure narrative; they have mobile exit (can shift to other political platforms, but only by changing identity as nationalist actors). Receiving-country publics: d = 0.50 (symmetric). Benefit from reduced job competition and symbolic security; pay through higher prices, reduced labor supply, and social cost of enforcement. The asymmetry between beneficiary and target seats is extreme; this is why the constraint cannot plausibly be claimed as rope (genuine coordination) and is properly claimed as snare.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (how to administer and track the population for conscription, taxation, and basic governance) is DEAD. Modern states track residents via digital identity systems decoupled from border closure. The apparatus persists through political capture: nationalist movements benefit electorally from restrictive messaging; state bureaucracies maintain budgets and legitimacy by continuing enforcement; labor-market-insiders benefit from wage protection. The constraint is a textbook example of mandatrophy resolution: the mandate is obsolete, but the constraint persists as rent-seeking. Under the freedom-of-movement reading, this mandatrophy is visible as illegitimacy — there is no legitimate founding problem, only a continued extraction mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_right_vs_constructed_category,
    'Is freedom of movement a natural right that precedes and limits sovereignty, or is it a constructed norm that states can legitimately restrict?',
    'Genealogical and anthropological evidence: do all human societies recognize some form of mobility right, or is the right culturally contingent? Cross-cultural examination of restrictions and their justifications. Legal-philosophy analysis of foundational texts (UN Charter, UDHR, state constitutions).',
    'If freedom of movement is universal and natural (transcends cultural variation), then border closure is a violation of a pre-political right and the constraint is snare-class. If it is contingent, then states can legitimately construct it otherwise, and the reading''s foundational premise collapses.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_right_vs_constructed_category, conceptual, 'Whether freedom of movement is natural or constructed.').

omega_variable(
    identity_lock_persistence_postexil,
    'For asylum seekers classified as identity-locked, what happens to the internalized suppression (the belief that they deserve exclusion, that their claim is illegitimate) if the exclusion mechanism is removed?',
    'Post-policy-change trajectory: if border opening occurs (refugee resettlement scale-up, labor-migration legalization), observe whether asylum seekers who had previously internalized rejection recover a sense of belonging or whether the internalization persists despite legal access.',
    'If suppression persists after access is granted, the constraint''s internalization is higher than measured suppression suggests. If suppression evaporates, it was purely structural. This determines the effective cost of fixing the constraint — internalized suppression requires psychological recovery infrastructure beyond policy change.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_persistence_postexil, empirical, 'Structural vs. internalized suppression mechanism for identity-locked asylum seekers.').

omega_variable(
    cosmopolitan_vs_nationalist_axiom_dispute,
    'What resolves the axiomatic dispute between the freedom-of-movement reading (cosmopolitan, personhood-primary) and the sovereignty-primary reading (nationalist, citizenship-primary)?',
    'No empirical data resolves this. The dispute is located in the foundational axioms: which category (personhood or citizenship) is primary? Which is the legitimate basis for rights distribution? This is a preference question, not a fact question. Resolution would require a meta-normative choice (e.g., a global referendum, or acceptance of a particular philosophical tradition as authoritative).',
    'This omega documents that the divergence between readings is not resolvable by investigation — it is constitutive of the kernel''s contestedness. Different jurisdictions and philosophical traditions will author the constraint differently. The engine''s per-seat classification maps this unresolvability as reading-dependent classification divergence.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(cosmopolitan_vs_nationalist_axiom_dispute, preference, 'Axiomatic/foundational normative disagreement between readings — unresolvable by empirical evidence.').

omega_variable(
    smuggling_network_rationality,
    'Are smuggling networks and underground labor markets rational responses to the constraint (substituting for blocked legal pathways), or are they pathologies that prove the need for restriction?',
    'Comparative analysis: in jurisdictions with more open labor migration, do smuggling networks shrink (supporting the substitution hypothesis) or persist at lower scale? Do smugglers target the same populations or different ones?',
    'If networks are rational substitutes, their existence proves the constraint is coercive — people want to move and will pay any cost to do so. The constraint''s persistence depends on preventing these rational substitutes from functioning openly. If networks are pathologies decoupled from restriction (people smuggle regardless of policy), then the case for opening borders as a solution is weaker.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(smuggling_network_rationality, empirical, 'Whether smuggling networks are rational responses to border closure or independent pathologies.').

omega_variable(
    reading_contingency_via_kernel,
    'This constraint is one reading of the border_control_legitimacy kernel. The sibling readings (sovereignty_primary, jurisdictional_sovereignty) would author the same constraint (border closure apparatus) with different ε values, different beneficiary/victim distributions, and different classifications. What makes each reading structurally coherent, and what would change a party''s commitment from one reading to another?',
    'Genealogical trace of legal doctrines and policy shifts: when do states shift from sovereignty_primary to jurisdictional_sovereignty framing? What triggers the shift? Track the change in founding-problem narrative and enforcement justification across historical transitions (e.g., post-WWII human-rights era, post-Cold-War globalization, post-2015 refugee crisis).',
    'Understanding the conditions under which readings change is diagnostic for mandatrophy and inertia: is the freedom_of_movement reading gaining coherence (increasing number of states adopting it) or losing it? Is the sovereignty_primary reading defending itself through intensified theater or genuine functional necessity? The trajectory informs which reading is becoming dominant.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_contingency_via_kernel, empirical, 'Conditions and transitions between kernel readings; coherence trajectory of this reading over time.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(border_control_legitimacy__freedom_of_movement_primary, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bord_tr_t0, border_control_legitimacy__freedom_of_movement_primary, theater_ratio, 0, 0.35).
narrative_ontology:measurement_basis(bord_tr_t0, projected).
narrative_ontology:measurement(bord_tr_t5, border_control_legitimacy__freedom_of_movement_primary, theater_ratio, 5, 0.4).
narrative_ontology:measurement_basis(bord_tr_t5, observed).
narrative_ontology:measurement(bord_tr_t10, border_control_legitimacy__freedom_of_movement_primary, theater_ratio, 10, 0.43).
narrative_ontology:measurement_basis(bord_tr_t10, observed).
narrative_ontology:measurement(bord_tr_t15, border_control_legitimacy__freedom_of_movement_primary, theater_ratio, 15, 0.46).
narrative_ontology:measurement_basis(bord_tr_t15, observed).
narrative_ontology:measurement(bord_tr_t20, border_control_legitimacy__freedom_of_movement_primary, theater_ratio, 20, 0.48).
narrative_ontology:measurement_basis(bord_tr_t20, observed).
narrative_ontology:measurement(bord_tr_t25, border_control_legitimacy__freedom_of_movement_primary, theater_ratio, 25, 0.48).
narrative_ontology:measurement_basis(bord_tr_t25, observed).
narrative_ontology:measurement(bord_tr_t35, border_control_legitimacy__freedom_of_movement_primary, theater_ratio, 35, 0.48).
narrative_ontology:measurement_basis(bord_tr_t35, observed).

% Extraction over time
narrative_ontology:measurement(bord_be_t0, border_control_legitimacy__freedom_of_movement_primary, base_extractiveness, 0, 0.68).
narrative_ontology:measurement_basis(bord_be_t0, projected).
narrative_ontology:measurement(bord_be_t5, border_control_legitimacy__freedom_of_movement_primary, base_extractiveness, 5, 0.72).
narrative_ontology:measurement_basis(bord_be_t5, observed).
narrative_ontology:measurement(bord_be_t10, border_control_legitimacy__freedom_of_movement_primary, base_extractiveness, 10, 0.76).
narrative_ontology:measurement_basis(bord_be_t10, observed).
narrative_ontology:measurement(bord_be_t15, border_control_legitimacy__freedom_of_movement_primary, base_extractiveness, 15, 0.79).
narrative_ontology:measurement_basis(bord_be_t15, observed).
narrative_ontology:measurement(bord_be_t20, border_control_legitimacy__freedom_of_movement_primary, base_extractiveness, 20, 0.81).
narrative_ontology:measurement_basis(bord_be_t20, observed).
narrative_ontology:measurement(bord_be_t25, border_control_legitimacy__freedom_of_movement_primary, base_extractiveness, 25, 0.82).
narrative_ontology:measurement_basis(bord_be_t25, observed).
narrative_ontology:measurement(bord_be_t35, border_control_legitimacy__freedom_of_movement_primary, base_extractiveness, 35, 0.82).
narrative_ontology:measurement_basis(bord_be_t35, observed).

% Suppression requirement over time
narrative_ontology:measurement(bord_su_t0, border_control_legitimacy__freedom_of_movement_primary, suppression_requirement, 0, 0.62).
narrative_ontology:measurement_basis(bord_su_t0, projected).
narrative_ontology:measurement(bord_su_t5, border_control_legitimacy__freedom_of_movement_primary, suppression_requirement, 5, 0.67).
narrative_ontology:measurement_basis(bord_su_t5, observed).
narrative_ontology:measurement(bord_su_t10, border_control_legitimacy__freedom_of_movement_primary, suppression_requirement, 10, 0.71).
narrative_ontology:measurement_basis(bord_su_t10, observed).
narrative_ontology:measurement(bord_su_t15, border_control_legitimacy__freedom_of_movement_primary, suppression_requirement, 15, 0.73).
narrative_ontology:measurement_basis(bord_su_t15, observed).
narrative_ontology:measurement(bord_su_t20, border_control_legitimacy__freedom_of_movement_primary, suppression_requirement, 20, 0.75).
narrative_ontology:measurement_basis(bord_su_t20, observed).
narrative_ontology:measurement(bord_su_t25, border_control_legitimacy__freedom_of_movement_primary, suppression_requirement, 25, 0.76).
narrative_ontology:measurement_basis(bord_su_t25, observed).
narrative_ontology:measurement(bord_su_t35, border_control_legitimacy__freedom_of_movement_primary, suppression_requirement, 35, 0.76).
narrative_ontology:measurement_basis(bord_su_t35, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(border_control_legitimacy__freedom_of_movement_primary, identity_coordination).
narrative_ontology:boltzmann_floor_override(border_control_legitimacy__freedom_of_movement_primary, 0.12).
narrative_ontology:affects_constraint(border_control_legitimacy__freedom_of_movement_primary, border_control_legitimacy__sovereignty_primary).
narrative_ontology:affects_constraint(border_control_legitimacy__freedom_of_movement_primary, border_control_legitimacy__jurisdictional_sovereignty).
narrative_ontology:affects_constraint(border_control_legitimacy__freedom_of_movement_primary, asylum_institutional_processing_constraint).
narrative_ontology:affects_constraint(border_control_legitimacy__freedom_of_movement_primary, labor_market_closure_constraint).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the border_control_legitimacy kernel. The same material arrangement (border closure apparatus) is authored with structurally different ε values, beneficiary/victim distributions, and types depending on the reading's axiomatic commitments. The freedom_of_movement_primary reading (this file) treats border closure as snare-class extraction. The sovereignty_primary reading treats it as natural law or rope-class coordination. The jurisdictional_sovereignty reading treats it as tangled_rope. All three stories describe the same apparatus; the readings differ on whether freedom of movement is a foundational human right that limits sovereignty (this reading), whether sovereignty includes border closure as a primary prerogative (sovereignty_primary), or whether legitimacy is contingent on balancing closure against protection and labor obligations (jurisdictional_sovereignty). Network edges link all three; per-seat classifications diverge by reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(border_control_legitimacy__freedom_of_movement_primary, powerless, 0.98).
constraint_indexing:directionality_override(border_control_legitimacy__freedom_of_movement_primary, institutional, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
