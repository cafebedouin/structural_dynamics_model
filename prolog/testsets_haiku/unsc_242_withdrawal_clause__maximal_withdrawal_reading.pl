% ============================================================================
% CONSTRAINT STORY: unsc_242_withdrawal_clause__maximal_withdrawal_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unsc_242_withdrawal_clause__maximal_withdrawal_reading, []).

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
 *   constraint_id: unsc_242_withdrawal_clause__maximal_withdrawal_reading
 *   human_readable: UNSC 242 Mandatory Withdrawal from All Occupied Territories (Maximal Reading)
 *   domain: international_law/diplomatic_history/treaty_interpretation
 *
 * SUMMARY:
 *   United Nations Security Council Resolution 242 (1967) calls for
 *   'withdrawal of Israeli armed forces from territories occupied in the
 *   recent conflict.' The maximal reading — the constraint story instantiated
 *   here — interprets this language through the UN Charter Article 2(4)
 *   territorial integrity default, privileging the French definite article
 *   'les' (the territories) over the English indefinite article 'any', and
 *   treats withdrawal as MANDATORY and COMPREHENSIVE: the occupying state
 *   bears an unconditional obligation to vacate all occupied land. No
 *   negotiation, no exceptions, no retention for security. This reading
 *   benefits dispossessed claimants by vesting them with an enforceable
 *   international entitlement; it constrains the occupier by foreclosing
 *   discretionary territory retention. The constraint is CLAIMED as rope (a
 *   binding coordination mechanism) but MEASURED as highly extractive
 *   (ε=0.82) because the obligation is one-directional and carries enormous
 *   state costs. The measurement series models enforcement intensification
 *   over 60 years: extractiveness rose from 0.68 (when the interpretation
 *   competed openly) to 0.82 (as institutional consensus hardened around the
 *   maximal reading). Theater ratio rose modestly (0.12 to 0.28), reflecting
 *   growing performance of withdrawal talk unmatched by enforcement.
 *   Suppression requirement climbed steadily, modeling the occupier's need to
 *   suppress the constraint's operation through defiance, diplomatic
 *   resistance, and institutional capture.
 *
 * KEY AGENTS:
 *   - Dispossessed territorial claimants: low-to-moderate power, generational horizon, constrained exit. Benefit from the maximal reading as the legal entitlement holder.
 *   - Occupying military state: institutional power, biographical horizon, constrained exit. Bears the obligation to withdraw; this is the seat that experiences extraction.
 *   - Drafting states (UN Charter authors): institutional power, civilizational horizon, analytical exit. Set the original norm; their intent is contested.
 *   - International Court of Justice: institutional power, generational horizon, analytical exit. Holds interpretive authority to certify the maximal reading.
 *   - Occupier's security coalition: powerful, biographical horizon, constrained exit. Excluded from the preferred interpretation; second-order payers if withdrawal is enforced.
 *   - Liberation and self-determination movements: organized power, generational horizon, constrained exit. Beneficiaries (alongside claimants) from the binding nature of the maximal reading.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unsc_242_withdrawal_clause__maximal_withdrawal_reading, 0.82).
domain_priors:suppression_score(unsc_242_withdrawal_clause__maximal_withdrawal_reading, 0.71).
domain_priors:theater_ratio(unsc_242_withdrawal_clause__maximal_withdrawal_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__maximal_withdrawal_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__maximal_withdrawal_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__maximal_withdrawal_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__maximal_withdrawal_reading, accessibility_collapse, 0.79).
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__maximal_withdrawal_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unsc_242_withdrawal_clause__maximal_withdrawal_reading, rope).
narrative_ontology:human_readable(unsc_242_withdrawal_clause__maximal_withdrawal_reading, "UNSC 242 Mandatory Withdrawal from All Occupied Territories (Maximal Reading)").
narrative_ontology:topic_domain(unsc_242_withdrawal_clause__maximal_withdrawal_reading, "international_law/diplomatic_history/treaty_interpretation").

domain_priors:requires_active_enforcement(unsc_242_withdrawal_clause__maximal_withdrawal_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unsc_242_withdrawal_clause__maximal_withdrawal_reading, '2d536a0d-7455-4782-ad22-3b8be0b925cf').
narrative_ontology:cs_kernel_codification('2d536a0d-7455-4782-ad22-3b8be0b925cf', fixed_text).
narrative_ontology:cs_authority_grounding('2d536a0d-7455-4782-ad22-3b8be0b925cf', lineage).
narrative_ontology:cs_interpretation_layer_present('2d536a0d-7455-4782-ad22-3b8be0b925cf').
narrative_ontology:cs_reading_relation('2d536a0d-7455-4782-ad22-3b8be0b925cf', unsc_242_withdrawal_clause__partial_withdrawal_reading, forecloses).
narrative_ontology:cs_reading_relation('2d536a0d-7455-4782-ad22-3b8be0b925cf', unsc_242_withdrawal_clause__interpretive_authority_structure, coexists_with).
narrative_ontology:cs_axiom('2d536a0d-7455-4782-ad22-3b8be0b925cf', foundational, mandatory_full_withdrawal_from_all_territories).
narrative_ontology:cs_axiom_status(mandatory_full_withdrawal_from_all_territories, holdable).
narrative_ontology:cs_axiom_grounding('2d536a0d-7455-4782-ad22-3b8be0b925cf', mandatory_full_withdrawal_from_all_territories, conventional).
narrative_ontology:cs_axiom('2d536a0d-7455-4782-ad22-3b8be0b925cf', foundational, french_text_semantic_precision_controls_ambiguity).
narrative_ontology:cs_axiom_status(french_text_semantic_precision_controls_ambiguity, holdable).
narrative_ontology:cs_axiom_grounding('2d536a0d-7455-4782-ad22-3b8be0b925cf', french_text_semantic_precision_controls_ambiguity, empirically_contingent).
narrative_ontology:cs_reference_frame('2d536a0d-7455-4782-ad22-3b8be0b925cf', charter_article_2_4_territorial_integrity_mandatory_default).
narrative_ontology:cs_drift_state('2d536a0d-7455-4782-ad22-3b8be0b925cf', contemporary_occupation_persistence, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('2d536a0d-7455-4782-ad22-3b8be0b925cf', '').
narrative_ontology:cs_kernel_id(unsc_242_withdrawal_clause__maximal_withdrawal_reading, unsc_242_withdrawal_clause).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unsc_242_withdrawal_clause__maximal_withdrawal_reading, dispossessed_territorial_claimants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(unsc_242_withdrawal_clause__maximal_withdrawal_reading, liberation_and_self_determination_movements).
narrative_ontology:constraint_victim(unsc_242_withdrawal_clause__maximal_withdrawal_reading, occupying_military_state).
narrative_ontology:constraint_victim(unsc_242_withdrawal_clause__maximal_withdrawal_reading, occupier_security_coalition).
narrative_ontology:constraint_vindicates(unsc_242_withdrawal_clause__maximal_withdrawal_reading, charter_article_2_4_territorial_integrity).
narrative_ontology:constraint_vindicates(unsc_242_withdrawal_clause__maximal_withdrawal_reading, pacta_sunt_servanda_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold legal claims to territories occupied in armed conflict. Under the maximal withdrawal reading, they possess an enforceable Charter-backed entitlement to full retrocession without precondition or negotiation. Their territorial claim is their core political and national identity; exit from the claim is exit from statehood. They benefit from the maximal reading because it provides the strongest possible legal position: the occupier's withdrawal is mandatory, not conditional on their concessions or security guarantees to the occupier. They cannot enforce the reading themselves (lack military superiority over occupier); they depend on UN organs or third-party enforcement. The constraint benefits them by establishing their legal entitlement, even if actual compliance is not forthcoming.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__maximal_withdrawal_reading, dispossessed_territorial_claimants, beneficiary,
    moderate, generational, constrained, global).

% Bears the obligation under the maximal reading to withdraw from all occupied territories unconditionally and permanently. Cannot negotiate territory retention in exchange for recognition, security guarantees, or peace treaties that would leave it militarily secured; the maximal reading treats withdrawal as non-negotiable. Must absorb all strategic, security, and domestic political costs of full retrocession: loss of territorial buffer, shift in regional military balance, domestic opposition from settlers and military factions. Exit options: (1) reject the maximal reading and argue for the partial reading or authority-structure reading (dispute the interpretation), (2) comply with the obligation and withdraw (incur massive domestic/strategic costs), (3) violate the obligation and suppress the constraint (accept legal isolation, sanctions, ICJ judgment, General Assembly condemnation). The occupier cannot exit occupation without either enforcing the constraint against itself or defeating the interpretation entirely.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__maximal_withdrawal_reading, occupying_military_state, payer,
    institutional, biographical, constrained, global).

% Codified Article 2(4) in the UN Charter at San Francisco (1945) with the intention to prevent aggressive war and enforce territorial integrity. They set the norm, but their intent is now disputed: maximalists read Article 2(4) as creating mandatory withdrawal from all occupied territories; minimalists read it as creating a principle subject to negotiated exceptions. The drafting states cannot unilaterally amend the Charter or override its plain language without consensus among the permanent members and a supermajority of the General Assembly. Their role as agenda-setter is complicated by: (1) some drafting states have themselves become occupiers or occupier-allies, making them biased interpreters of their own text, (2) the text is genuinely ambiguous (French vs. English, definite vs. indefinite article), (3) the interpretive authority has migrated from drafters to the ICJ, so they can attest intent but cannot control interpretation. They sit in the agenda-setter role structurally (they wrote the law) but lack contemporary enforcement authority over it.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__maximal_withdrawal_reading, united_nations_drafting_states, agenda_setter,
    institutional, civilizational, analytical, universal).

% Holds the seat of interpretive authority over the Charter and UNSC Resolution 242 in contentious cases and advisory opinions. The ICJ can certify the maximal withdrawal reading as binding law through a judgment that declares Article 2(4) mandates full withdrawal. However, the ICJ's authority is itself contested: the occupying state may reject its jurisdiction or refuse to comply with its judgment; other states may argue that the General Assembly has concurrent interpretive authority; the drafting states may argue that authorial intent (their intent) should override textual interpretation. The ICJ is not a payer or beneficiary — it is the authoritative interpreter seat. It bears the burden of interpretation (must read the text carefully and justify the reading) but does not itself suffer the costs of the interpretation (the occupier does). The observer role reflects analytical detachment: the Court's stake is in the integrity of law and interpretation procedure, not in the material outcome of withdrawal or retention.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__maximal_withdrawal_reading, international_court_of_justice, observer,
    institutional, generational, analytical, universal).

% States politically and militarily aligned with the occupier (provide military aid, diplomatic protection, UN Security Council veto). Under the maximal reading, their preferred interpretation (partial withdrawal or authority-structure dispute) is not the binding one; the constraint forecloses their preferred settlement. If the maximal reading is enforced and the occupier withdraws, the coalition loses: (1) a strategic military ally's territorial buffer in a contested region, (2) geopolitical influence from the occupier's regional power, (3) alignment credibility (they backed an occupier who withdrew anyway). They are payers in the sense that they bear secondary costs of enforcement, but they are not primary targets — the occupier is the primary payer. They are partly excluded from the interpretation process: their preferred reading is not the governing one; they have political voice (UN Security Council, alliance influence) but not the controlling interpretation. They would mobilize to argue for the partial_withdrawal_reading or to block enforcement of the maximal reading.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__maximal_withdrawal_reading, occupier_security_coalition, payer,
    powerful, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(unsc_242_withdrawal_clause__maximal_withdrawal_reading, occupier_security_coalition, excluded).

% Non-state actors (national liberation movements, civil society organizations, human rights NGOs) whose organizational mandate includes enforcing decolonization and opposing occupation. They benefit from the maximal withdrawal reading because it provides a binding international legal framework that declares occupation unlawful and withdrawal mandatory. Their organizational identity is constituted through commitment to the self-determination principle; exit from that principle is exit from their raison d'être. They lack formal standing in the ICJ (cannot file cases directly) but can mobilize UN General Assembly support, organize grassroots movements, and generate public and diplomatic pressure to enforce the maximal reading. Their role is beneficiary rather than payer, and their power is organized (coalition of movements) rather than institutional (no formal state authority), but they are key to sustaining the reading's legitimacy and generating pressure for enforcement.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__maximal_withdrawal_reading, liberation_and_self_determination_movements, beneficiary,
    organized, generational, constrained, global).

% States without direct territorial stakes in this specific conflict (non-aligned nations, nations with different territorial disputes), and academic/legal experts from international law, diplomacy, and history. They serve as observers and commentators, providing expert testimony before the ICJ, publishing doctrinal analysis, voting in the UN General Assembly on withdrawal resolutions, and shaping scholarly consensus about what the Charter means. Their role is not to pay or benefit materially; it is to generate the expert and political legitimacy that certifies or contests the maximal reading. A scholarly consensus supporting the maximal reading increases its perceived bindingness; lack of consensus allows the occupier and its allies to argue the reading is contestable. Their power is diffuse but real: expert credibility shapes what states claim about the law, and what states claim shapes enforcement.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__maximal_withdrawal_reading, neutral_states_and_legal_scholars, observer,
    organized, generational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(unsc_242_withdrawal_clause__maximal_withdrawal_reading, diffuse).
narrative_ontology:fixing_cost_class(unsc_242_withdrawal_clause__maximal_withdrawal_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a binding international obligation to restore territorial integrity after armed conflict, preventing indefinite occupation and creating a legal mechanism (ICJ jurisdiction, UN enforcement organs) for settling territorial disputes. The coordination problem is: how do states cooperate to make occupation illegitimate and revert territorial claims to their pre-conflict state? The answer is a Charter obligation to withdraw.
% TRANSFER_FUNCTION: Transfers legal authority to determine boundary restoration from the occupying military state to the international legal system (the UN organs and ICJ). The maximal reading strips the occupier of discretion to retain territory and vests the claimant with an enforceable entitlement. The transfer is of institutional power and legitimacy, not of material goods — but the material consequence is that territories flow back to the claimant rather than remaining under occupation.
% ABSENT_VOICES: The occupying state disputes the interpretation but is not absent from the conversation — it negotiates over the scope. Truly absent voices: dispossessed populations whose territorial interest is recognized but who lack formal standing in treaty interpretation; smaller occupiers who would be bound but who have less political power to contest the reading in UN forums; scholars from occupier-aligned states who dispute the French-text reading but whose work is marginalized in mainstream interpretation discourse.
% DISAPPEARANCE_RATIONALE: If this interpretation vanished and the maximal reading were replaced by the partial reading or defeated entirely, occupied territories would remain under military control indefinitely, UN enforcement mechanisms would collapse, and the international legal structure for territorial restoration would evaporate. Claimant states would shift to military reconquest or protracted legal stalemate rather than relying on Charter obligation. The geopolitical map would crystallize around occupation rather than restoring pre-conflict boundaries.
% FOUNDING_PROBLEM: The UN Charter's Article 2(4) was drafted in 1945 to prevent aggressive war and to obligate states to respect each other's territorial integrity. The founding problem: how do we make occupation illegitimate and bind states to withdraw from conquered territory? The answer enshrined was a Charter obligation.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by the Charter's own language and by historical drafting records that confirm territorial integrity and prohibition of force were core provisions. HOWEVER, the status of that problem — whether withdrawal obligation is live and mandatory, or discretionary and negotiable — is disputed. The occupying state contests that the maximal reading reflects the founding problem; it argues the drafting also preserved negotiation and security exceptions. Corroboration from outside the claimant set: the ICJ in Advisory Opinion on the Legality of the Threat or Use of Nuclear Weapons (1996) and in Legality of the Use of Force cases affirmed that territorial integrity is a foundational Charter principle; however, these opinions stopped short of declaring mandatory withdrawal without exception, leaving the scope ambiguous. Independent legal scholarship is split, with maximalist commentators (Brownlie, Koskenniemi) supporting full withdrawal and minimalist commentators (Jennings, Aust) arguing for negotiated settlement. The founding problem REMAINS live in the sense that territorial integrity is still a core value, but whether it mandates unconditional withdrawal is the very dispute this reading instantiates.
narrative_ontology:disappearance_verdict(unsc_242_withdrawal_clause__maximal_withdrawal_reading, world_rearranges).
narrative_ontology:founding_problem_status(unsc_242_withdrawal_clause__maximal_withdrawal_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unsc_242_withdrawal_clause__maximal_withdrawal_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(unsc_242_withdrawal_clause__maximal_withdrawal_reading, 'none', 1).
narrative_ontology:epsilon_provenance(unsc_242_withdrawal_clause__maximal_withdrawal_reading, 0.82, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unsc_242_withdrawal_clause__maximal_withdrawal_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(unsc_242_withdrawal_clause__maximal_withdrawal_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(unsc_242_withdrawal_clause__maximal_withdrawal_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.82 at interval end) because the maximal reading strips the occupier of all discretion: withdrawal is not negotiable, not conditional on security guarantees or recognition, not subject to 'safe boundaries' exceptions. The obligation is imposed unilaterally by the interpretation, with no offsetting benefit to the occupier. Suppression is substantial (0.71) because the occupier must actively resist, deny, reinterpret, and suppress the constraint's operation to maintain occupation — the constraint cannot persist by consent or benignity. Theater ratio is low-moderate (0.28): the withdrawal language is real (states do speak withdrawal), but the ratio reflects that a large share of enforcement effort is spent on performative gestures (peace process theater, conditional negotiation framing) rather than actual territorial surrender. The measurement series run on a unified grid: all three metrics are authored at time points 0, 10, 20, 30, 45, 60, so every metric has a value everywhere. Extractiveness and suppression_requirement both rise over the interval (interpretation hardens), while theater_ratio rises more slowly (performance increases but real enforcement lags). The shape models interpretation consolidation without matched enforcement — a constraint gaining in institutional bindingness while actual compliance remains suppressed.
 *
 * PERSPECTIVAL GAP:
 *   The dispossessed claimant and the occupier experience this constraint entirely differently. From the claimant's seat: the constraint is a binding entitlement, a rope that coordinates international will around territory restoration, a framework that vests them with legal standing. They compute the constraint as coordination-with-enforcement. From the occupier's seat: the constraint is an imposed obligation with no offsetting benefit, a rope that binds but provides no exit, coordination that extracts everything from them. The occupier computes it as extraction under legal cover. The drafting states and the ICJ occupy the interpretive seat: for them, the constraint is a question of reading (what does the text say?), not primarily a question of benefit/burden. The security coalition that backs the occupier experiences secondary extraction: if the maximal reading is enforced, they lose a military ally's territorial buffer and face regional balance shift. The key gap is between the agenda-setter's framing (this is what the Charter requires) and the payer's framing (this is an occupation of interpretive authority, using legal text to impose a predetermined outcome). The engine computes this divergence from power, exit, and beneficiary/victim declarations; the authored claim (rope) does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Dispossessed claimants are beneficiaries (the maximal reading gives them enforceable entitlement) with moderate power and constrained exit (they cannot exit the territorial claim without ceasing to be claimants). Their directionality is close to zero or slightly negative — they benefit from the constraint and would suffer its removal. The occupier is a payer (forced to withdraw, loses strategic territory, bears diplomatic isolation if it defies) with institutional power and constrained exit (cannot exit occupation without enforcing the constraint against itself). Its directionality is high (near 1.0) — the constraint extracts from it. The drafting states and ICJ sit near d=0.5 (symmetric): they bear interpretive labor but claim no material stake. The security coalition backing the occupier carries mixed directionality: they are payers (regional instability if the occupier withdraws) but not named as a primary victim (the analysis treats the occupier as the main seat bearing the cost). No directionality overrides are necessary here: the structural derivation from beneficiary/victim data and exit options produces accurate d values for each seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The maximal withdrawal reading raises a mandatrophy question: the founding problem (preventing indefinite occupation, enforcing territorial integrity) is STILL LIVE — but the mandate to solve it has ATROPHIED. The UN organs tasked with enforcing Article 2(4) have not compelled withdrawal; the ICJ has not rendered a definitive judgment ordering mandatory full retrocession; claimant states have not mobilized military or economic enforcement; the occupier has not withdrawn. After 60 years, the interpretation is institutionally hardened (accepted in scholarly consensus, endorsed in General Assembly resolutions) but operationally DEAD — the constraint persists as principle but not as function. This is not a rope (which solves a coordination problem through binding agreement and mutual benefit). It is not quite a piton (which requires no one to benefit enough to maintain it — here claimants benefit materially from enforcement). Rather, it is a SCAFFOLD with a blocked sunset: the constraint was meant to be transitional (resolution → withdrawal → peace), but the transition never completed, and the constraint's function (forcing restoration) has been replaced by its performance (condemning occupation while accepting its persistence). The mandatrophy resolution: this reading instantiates a ROPE whose mandate has DECAYED but whose form persists because claimants lack enforcement power, the occupier suppresses it, and the international community performs commitment without delivering it. The classification (rope) holds structurally; the atrophied operation is what the measurement series and theater_ratio capture.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    french_vs_english_text_privilege,
    'Does the French definite article ''les'' (the territories) control interpretation, or does the English indefinite ''any'' represent the drafters'' intent equally?',
    'Drafting history analysis (Travaux préparatoires), ICJ methodology for multilingual treaty interpretation (Vienna Convention Article 33), expert linguistic testimony on whether French definiteness is a meaningful semantic difference.',
    'If French text is privileged: mandatory withdrawal from ALL territories (maximal reading, this constraint). If English text is equally authoritative: withdrawal from ''any'' territories could mean partial, negotiated withdrawal (partial reading). The scope of the obligation flips.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(french_vs_english_text_privilege, empirical, 'Which language version of the Charter text controls interpretation of mandatory withdrawal scope.').

omega_variable(
    binding_authority_of_charter_article_2_4_default,
    'Does Article 2(4) establish a default rule binding all UN member states, or does it establish a principle subject to negotiated exceptions in specific regional conflicts?',
    'ICJ interpretation of Article 2(4) in contentious cases; UN General Assembly voting patterns on withdrawal resolutions; state practice in post-conflict settlements (do states negotiate territory, or invoke mandatory withdrawal?); doctrine consensus in mainstream international law scholarship.',
    'If it establishes a binding default: withdrawal is mandatory unless explicitly superseded by the Security Council (maximal reading). If it establishes a principle subject to negotiated exceptions: withdrawal scope becomes discretionary (partial reading).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(binding_authority_of_charter_article_2_4_default, conceptual, 'Whether Charter Article 2(4) territorial integrity is a binding rule or a negotiable principle.').

omega_variable(
    interpreting_authority_displacement,
    'Who has ultimate authority to interpret UNSC Resolution 242 and Article 2(4): the ICJ (judicial interpretation), the UN drafting states (authorial intent), the occupying state (customary practice under de facto control), or a consensus of the UN General Assembly (collective interpretation)?',
    'Formal ICJ advisory opinion or contentious judgment declaring interpretive authority; UN Charter amendment clarifying interpretation procedures; state practice consolidation around one authoritative interpreter.',
    'If ICJ: the maximal reading can be certified as binding law. If drafting states: the intent becomes the standard, and if drafters intended discretionary withdrawal, the maximal reading fails. If occupying state: occupation generates facts on the ground that supersede the textual reading (effective authority replaces legal authority). If General Assembly: majority voting determines the reading, which could shift as bloc alignments change.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(interpreting_authority_displacement, conceptual, 'Who is the legitimate interpreter of the Charter and UNSC Resolution 242 — the seat that can authoritatively declare what the constraint means.').

omega_variable(
    enforcement_mechanism_credibility,
    'Can the UN organs (Security Council, General Assembly, ICJ) actually compel withdrawal, or does enforcement capacity depend on military/political will that the constraint cannot generate?',
    'Historical enforcement record: cases where the UN has compelled state withdrawal (East Timor, Kosovo, Golan Heights if applicable); cases where it has failed (Palestinian territories, Crimea); analysis of enforcement machinery (does the Charter machinery exist, or does enforcement depend on ad hoc coalition?)',
    'If UN can compel: the constraint is binding and extractive (occupier must comply). If UN cannot: the constraint is performative (occupation persists despite the maximal reading). This shapes whether the constraint is rope (coordinates withdrawal) or piton (coordinates condemnation while accepting occupation).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(enforcement_mechanism_credibility, empirical, 'Whether the UN''s interpretation authority has backing to enforce mandatory withdrawal or whether enforcement depends on external coalition will.').

omega_variable(
    counterargument_secure_boundaries_principle,
    'Does a separate principle — ''secure boundaries'' or ''defensible borders'' — exist in international law as a competing default that can override mandatory withdrawal from all territories?',
    'Doctrinal analysis of secure/defensible boundaries in ICJ case law and treaty practice; historical instances where security concerns were used to justify partial withdrawal; comparison to other territorial disputes resolved by boundary compromise rather than full restoration.',
    'If secure boundaries is a live principle with equal standing: it provides a reading-theoretic door to partial withdrawal (the partial_withdrawal_reading becomes tenable). If it is a rejected or subsidiary principle: the maximal reading faces less textual competition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counterargument_secure_boundaries_principle, empirical, 'Whether international law recognizes a competing principle that permits territory retention for security, offsetting the mandatory withdrawal norm.').

omega_variable(
    beneficiary_enforcement_capacity,
    'Do the dispossessed claimants have sufficient power (military, political, institutional) to enforce the maximal withdrawal interpretation, or does enforcement depend on third-party (UN, allied state) capacity?',
    'Assessment of claimant state military capability, UN voting bloc alignment, historical military recourse by claimants, institutional leverage in UN organs.',
    'If claimants have enforcement capacity: the constraint is rope (coordination with beneficiary enforcement). If claimants lack capacity: the constraint is piton (claimants benefit from the principle but cannot enforce it, occupation persists by suppression, and the constraint''s function has atrophied). This shapes the extracted/coordinated boundary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_enforcement_capacity, empirical, 'Whether the beneficiary can enforce the constraint or whether enforcement must be delegated to the UN or external powers.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unsc_242_withdrawal_clause__maximal_withdrawal_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(unsc_tr_t0, unsc_242_withdrawal_clause__maximal_withdrawal_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(unsc_tr_t0, observed).
narrative_ontology:measurement(unsc_tr_t10, unsc_242_withdrawal_clause__maximal_withdrawal_reading, theater_ratio, 10, 0.16).
narrative_ontology:measurement_basis(unsc_tr_t10, observed).
narrative_ontology:measurement(unsc_tr_t20, unsc_242_withdrawal_clause__maximal_withdrawal_reading, theater_ratio, 20, 0.2).
narrative_ontology:measurement_basis(unsc_tr_t20, observed).
narrative_ontology:measurement(unsc_tr_t30, unsc_242_withdrawal_clause__maximal_withdrawal_reading, theater_ratio, 30, 0.25).
narrative_ontology:measurement_basis(unsc_tr_t30, observed).
narrative_ontology:measurement(unsc_tr_t45, unsc_242_withdrawal_clause__maximal_withdrawal_reading, theater_ratio, 45, 0.27).
narrative_ontology:measurement_basis(unsc_tr_t45, observed).
narrative_ontology:measurement(unsc_tr_t60, unsc_242_withdrawal_clause__maximal_withdrawal_reading, theater_ratio, 60, 0.28).
narrative_ontology:measurement_basis(unsc_tr_t60, projected).

% Extraction over time
narrative_ontology:measurement(unsc_be_t0, unsc_242_withdrawal_clause__maximal_withdrawal_reading, base_extractiveness, 0, 0.68).
narrative_ontology:measurement_basis(unsc_be_t0, observed).
narrative_ontology:measurement(unsc_be_t10, unsc_242_withdrawal_clause__maximal_withdrawal_reading, base_extractiveness, 10, 0.71).
narrative_ontology:measurement_basis(unsc_be_t10, observed).
narrative_ontology:measurement(unsc_be_t20, unsc_242_withdrawal_clause__maximal_withdrawal_reading, base_extractiveness, 20, 0.75).
narrative_ontology:measurement_basis(unsc_be_t20, observed).
narrative_ontology:measurement(unsc_be_t30, unsc_242_withdrawal_clause__maximal_withdrawal_reading, base_extractiveness, 30, 0.79).
narrative_ontology:measurement_basis(unsc_be_t30, observed).
narrative_ontology:measurement(unsc_be_t45, unsc_242_withdrawal_clause__maximal_withdrawal_reading, base_extractiveness, 45, 0.81).
narrative_ontology:measurement_basis(unsc_be_t45, observed).
narrative_ontology:measurement(unsc_be_t60, unsc_242_withdrawal_clause__maximal_withdrawal_reading, base_extractiveness, 60, 0.82).
narrative_ontology:measurement_basis(unsc_be_t60, projected).

% Suppression requirement over time
narrative_ontology:measurement(unsc_su_t0, unsc_242_withdrawal_clause__maximal_withdrawal_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(unsc_su_t0, observed).
narrative_ontology:measurement(unsc_su_t10, unsc_242_withdrawal_clause__maximal_withdrawal_reading, suppression_requirement, 10, 0.59).
narrative_ontology:measurement_basis(unsc_su_t10, observed).
narrative_ontology:measurement(unsc_su_t20, unsc_242_withdrawal_clause__maximal_withdrawal_reading, suppression_requirement, 20, 0.63).
narrative_ontology:measurement_basis(unsc_su_t20, observed).
narrative_ontology:measurement(unsc_su_t30, unsc_242_withdrawal_clause__maximal_withdrawal_reading, suppression_requirement, 30, 0.67).
narrative_ontology:measurement_basis(unsc_su_t30, observed).
narrative_ontology:measurement(unsc_su_t45, unsc_242_withdrawal_clause__maximal_withdrawal_reading, suppression_requirement, 45, 0.7).
narrative_ontology:measurement_basis(unsc_su_t45, observed).
narrative_ontology:measurement(unsc_su_t60, unsc_242_withdrawal_clause__maximal_withdrawal_reading, suppression_requirement, 60, 0.71).
narrative_ontology:measurement_basis(unsc_su_t60, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unsc_242_withdrawal_clause__maximal_withdrawal_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(unsc_242_withdrawal_clause__maximal_withdrawal_reading, 0.18).
narrative_ontology:affects_constraint(unsc_242_withdrawal_clause__maximal_withdrawal_reading, unsc_242_withdrawal_clause__partial_withdrawal_reading).
narrative_ontology:affects_constraint(unsc_242_withdrawal_clause__maximal_withdrawal_reading, unsc_242_withdrawal_clause__interpretive_authority_structure).

% DUAL FORMULATION NOTE:
% This constraint is a member of the unsc_242_withdrawal_clause constraint family. The kernel is UN Charter Article 2(4) territorial integrity and UNSC Resolution 242's withdrawal language. Three structurally distinct constraints result from three incompatible readings: (1) maximal_withdrawal_reading (this file) — mandatory full withdrawal, French text controls, high extractiveness. (2) partial_withdrawal_reading — discretionary scope, English text permits exceptions, medium extractiveness. (3) interpretive_authority_structure — the authority to resolve the text is itself contested, leading to meta-level conflict over which reading binds. Each reading instantiates a different constraint with different ε, different beneficiary structure, and different classification. They are linked by network.affects_constraints because the choice of reading determines the operational constraint; a shift from maximal to partial reading would alter the occupier's obligation scope and the claimant's enforceable entitlement. The maximal reading influences the partial reading (creates pressure toward full withdrawal interpretive frames) and coexists with the authority-structure constraint (they are different levels of contestation — one disputes the reading, the other disputes who gets to read).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
