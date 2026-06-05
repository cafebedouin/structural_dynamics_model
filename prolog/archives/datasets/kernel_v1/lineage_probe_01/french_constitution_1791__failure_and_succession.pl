% ============================================================================
% CONSTRAINT STORY: french_constitution_1791__failure_and_succession
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_french_constitution_1791__failure_and_succession, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: french_constitution_1791__failure_and_succession
 *   human_readable: The 1791 Constitution: Failure and Succession — Discarded First Draft of the Revolution
 *   domain: political/constitutional_history
 *
 * SUMMARY:
 *   The 1791 Constitution, formally the Constitution of the French People,
 *   lasted from September 1791 to August 1792 — less than a year. It was the
 *   Revolution's first attempt at a complete constitutional settlement, and
 *   its rapid collapse structured all subsequent revolutionary
 *   constitutionalism. The reading 'failure_and_succession' analyzes the
 *   constraint created by the constitution's impossibility: it promised
 *   constitutional monarchy (monarchy subordinated to law, royal veto reduced
 *   to suspensive form) to a monarchy that would not accept subordination
 *   (Louis XVI's secret dealings, the flight to Varennes, and preparation for
 *   war with revolutionary France). The constraint extracted from all parties
 *   who believed in the settlement: constitutional monarchists were trapped
 *   between an uncompromising crown and an increasingly radical public;
 *   moderates watched their legal framework dissolve as events overwhelmed
 *   constitutional containment; even the Assembly found itself interpreting
 *   and suspending its own text. The beneficiary was the radical republican
 *   faction, which mobilized in the constitutional void created by the
 *   monarchy's revealed unreliability and the war that the constitution could
 *   not manage. This reading is one of four structural readings of the same
 *   kernel (the 1791 Constitution itself): one reading examines the
 *   citizenship division (active/passive), another the Declaration of Rights
 *   that frames the text, another the surviving monarchy and its veto. This
 *   reading focuses on the text's temporal trajectory: what made it fail, and
 *   what that failure enabled.
 *
 * KEY AGENTS:
 *   - Constitutional Monarchy Adherents (trapped/powerless): Believers in constitutional limits on royal power, caught between a monarchy unwilling to be limited and a republic unwilling to compromise. Suppression total — no political future after the settlement's collapse.
 *   - Moderate Revolutionary Deputies (constrained/moderate): The Feuillant faction and moderate Assembly majority who designed the 1791 Constitution. Trapped by discovery that constitutional monarchy requires a monarch willing to accept permanent subordination — a structural impossibility revealed by Louis XVI's own actions.
 *   - Radical Republican Coalition (mobile/organized): Mobilized against the constitutional compromise. Beneficiary of the constitution's failure — their opposition was vindicated by events, and the constitutional void created space for republican consolidation.
 *   - Louis XVI and the Monarchy (institutional/arbitrage): The crown itself, engaged in secret negotiations with foreign powers while publicly swearing fidelity to the constitution. Created the structural incompatibility that destroyed the settlement.
 *   - The Legislative Assembly (institutional/arbitrage): The constitutive body that both designed the text and, ultimately, had to abandon it. Experienced the constraint as institutional self-contradiction.
 *   - War and International Conflict (analytical/trapped): The structural force that accelerated the constitution's impossibility — Austria and Prussia's threat made constitutional containment of revolutionary energy impossible.
 *   - The Declaration of Rights (analytical/analytical): The universalist framing (Declaration prefixed to the Constitution) that contrasted sharply with the constitution's actual particularism (active/passive citizenship, retained monarchy). This gap forced interpretation and amendment.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(french_constitution_1791__failure_and_succession, 0.68).
domain_priors:suppression_score(french_constitution_1791__failure_and_succession, 0.72).
domain_priors:theater_ratio(french_constitution_1791__failure_and_succession, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(french_constitution_1791__failure_and_succession, extractiveness, 0.68).
narrative_ontology:constraint_metric(french_constitution_1791__failure_and_succession, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(french_constitution_1791__failure_and_succession, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(french_constitution_1791__failure_and_succession, snare).
narrative_ontology:human_readable(french_constitution_1791__failure_and_succession, "The 1791 Constitution: Failure and Succession — Discarded First Draft of the Revolution").
narrative_ontology:topic_domain(french_constitution_1791__failure_and_succession, "political/constitutional_history").

domain_priors:requires_active_enforcement(french_constitution_1791__failure_and_succession).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(french_constitution_1791__failure_and_succession, '91177d7d-be7c-4068-b394-8b26809929f2').
narrative_ontology:cs_kernel_codification('91177d7d-be7c-4068-b394-8b26809929f2', formalized).
narrative_ontology:cs_authority_grounding('91177d7d-be7c-4068-b394-8b26809929f2', lineage).
narrative_ontology:cs_interpretation_layer_present('91177d7d-be7c-4068-b394-8b26809929f2').
narrative_ontology:cs_reading_relation('91177d7d-be7c-4068-b394-8b26809929f2', french_constitution_1791__active_passive_citizenship, coexists_with).
narrative_ontology:cs_reading_relation('91177d7d-be7c-4068-b394-8b26809929f2', french_constitution_1791__declaration_of_rights_1789, influences).
narrative_ontology:cs_reading_relation('91177d7d-be7c-4068-b394-8b26809929f2', french_constitution_1791__suspensive_veto_monarchy, coexists_with).
narrative_ontology:cs_axiom('91177d7d-be7c-4068-b394-8b26809929f2', foundational, constitutional_monarchy_requires_willing_subordination).
narrative_ontology:cs_axiom_status(constitutional_monarchy_requires_willing_subordination, holdable).
narrative_ontology:cs_axiom_grounding('91177d7d-be7c-4068-b394-8b26809929f2', constitutional_monarchy_requires_willing_subordination, empirically_contingent).
narrative_ontology:cs_axiom('91177d7d-be7c-4068-b394-8b26809929f2', foundational, revolution_cannot_coexist_with_restored_monarchy).
narrative_ontology:cs_axiom_status(revolution_cannot_coexist_with_restored_monarchy, holdable).
narrative_ontology:cs_axiom_grounding('91177d7d-be7c-4068-b394-8b26809929f2', revolution_cannot_coexist_with_restored_monarchy, empirically_contingent).
narrative_ontology:cs_reference_frame('91177d7d-be7c-4068-b394-8b26809929f2', constitutional_monarchy_as_stable_settlement).
narrative_ontology:cs_drift_state('91177d7d-be7c-4068-b394-8b26809929f2', august_1792_insurrection, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('91177d7d-be7c-4068-b394-8b26809929f2', '').
narrative_ontology:cs_kernel_id(french_constitution_1791__failure_and_succession, french_constitution_1791).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(french_constitution_1791__failure_and_succession, republic_faction_montagnard).
narrative_ontology:constraint_beneficiary(french_constitution_1791__failure_and_succession, radical_republican_coalition).
narrative_ontology:constraint_victim(french_constitution_1791__failure_and_succession, constitutional_monarchy_adherents).
narrative_ontology:constraint_victim(french_constitution_1791__failure_and_succession, moderate_revolutionary_settlement).
narrative_ontology:constraint_victim(french_constitution_1791__failure_and_succession, royalist_accommodation_attempt).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CONSTITUTIONAL MONARCHY ADHERENTS (SNARE) — Trapped by their own compromise. Believers in constitutional limits on royal power found themselves caught between a monarchy that would not be limited (Louis XVI's flight, secret negotiations with foreign powers) and a republic that would not compromise. No exit: too radical for royalists, too monarchist for republicans. Suppression total — they could neither defend their settlement nor abandon it without risking their lives. Pure extraction: their political vision was obliterated by the incompatibility of its own premises.
constraint_indexing:constraint_classification(french_constitution_1791__failure_and_succession, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MODERATE REVOLUTIONARY SETTLEMENT BUILDERS (SNARE) — Constrained by the incompatibility of their own project. Feuillants and moderate deputies designed a constitutional system that required the king to accept permanent subordination — a structural impossibility. They bore the extraction cost of discovering this incompatibility through failure: their reputation destroyed, their legislative majority dissolved, their constitutional text discarded within a year. Not trapped in the sense of the monarchy adherents, but constrained by forces (war, royal flight, radical mobilization) they did not create and could not control.
constraint_indexing:constraint_classification(french_constitution_1791__failure_and_succession, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: RADICAL REPUBLICAN COALITION (ROPE) — Benefits from the 1791 Constitution's failure as a pure coordination mechanism. The events that destroyed the monarchy compromise (war, Varennes, August insurrection) clarified the political problem: the republic and monarchy cannot coexist. The radicals experienced the constitution not as extractive but as obstacle-removal — its failure enabled the real coordination problem to be solved (abolishing the monarchy, drafting the 1792 Constitution). Effective extraction χ low because exit was available: radicals could and did exit the 1791 settlement and mobilize alternatives.
constraint_indexing:constraint_classification(french_constitution_1791__failure_and_succession, rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 4: THE LEGISLATIVE ASSEMBLY AS INSTITUTION (TANGLED ROPE) — Mixed coordination and extraction. The Assembly designed a text (coordination function: codifying rights, establishing separation of powers) but the text was also a trap: it created a veto for the crown that the crown would not use constitutionally, and it invested radical hope in a king who was in secret alliance with foreign invasion. The Assembly benefited from the constitutional project — it consolidated revolutionary gains, established legitimacy through law rather than terror — while also extracting costs from itself by binding its future to an impossible compromise. Active enforcement was required: the Assembly had to interpret, suspend, and ultimately abandon its own text as events progressed.
constraint_indexing:constraint_classification(french_constitution_1791__failure_and_succession, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER / STRUCTURAL NECESSITY VIEW (SCAFFOLD) — From a generational/national analytical perspective, the 1791 Constitution served as a temporary structure enabling the next phase of settlement. It was not a failed constitution but a provisional one: it accomplished what it could (crystallizing rights, establishing constitutional government, clarifying the monarchy question) and then dissolved when its premises became structurally incompatible with ongoing conflict. The constraint here is the constitution's own success at revealing its internal contradictions — it extracted from adherents by failing, but that failure was generative. Theater ratio is moderate (0.58): the constitutional ceremony was partly performative (the king's acceptance of limits he did not believe in, the Assembly's faith in constitutional containment of revolutionary energy) and partly functional (it did establish new legal norms, did institute representative government, did codify rights).
constraint_indexing:constraint_classification(french_constitution_1791__failure_and_succession, scaffold,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(national))).

% PERSPECTIVE 6: CONSTITUTIONAL MONARCHY AS INSTITUTIONAL FORM (PITON) — From a civilizational view, the 1791 Constitution represents a degraded institutional form: constitutional monarchy as an attempt to preserve monarchy while ceding power to constitutional limits. This form persisted through theater in monarchies that followed (Spain, Belgium, Japan), but the 1791 French version revealed its inherent weakness. The constraint is institutional inertia: the very concept of constitutional monarchy, which would later seem stable in other contexts, was here exposed as structurally impossible in a revolutionary moment. The theater ratio reflects that the constitutional performance — the ceremony of the king swearing an oath to a constitution that subordinated him — was maintained for months despite its logical impossibility. Piton classification: the institutional form persisted through its own ritual performance, not through structural function.
constraint_indexing:constraint_classification(french_constitution_1791__failure_and_succession, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(french_constitution_1791__failure_and_succession_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(french_constitution_1791__failure_and_succession, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(french_constitution_1791__failure_and_succession, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(french_constitution_1791__failure_and_succession, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(french_constitution_1791__failure_and_succession, TR),
    TR >= 0.70.

:- end_tests(french_constitution_1791__failure_and_succession_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High but not total. The constraint extracts from settlement believers by forcing a choice between a monarchy that will not be limited and a republic that will not compromise. But the extraction is not maximal because some benefit exists from the intermediate state: the 1791 Constitution did establish constitutional government, did codify rights, did reduce (even if not eliminate) royal power. The moderate extraction reflects that this was not pure coercion but structural incompatibility. The measurement trajectory (0.45 → 0.58 → 0.68) shows accumulating extractiveness as the contradiction becomes undeniable: initial hopes that the king would accept constitutional limits give way to evidence of his refusal (Varennes at month 3), and by month 6 the constitution is openly suspended, its beneficiaries in control, its defenders politically destroyed. Suppression (0.72): High. The monarchy actively suppressed alternatives to the 1791 settlement during its brief life — secret alliances, refusal to acknowledge the constitution's logic, preparation for war to overturn it. Settlement believers suppressed radical alternatives to avoid accelerating collapse. War suppressed the Assembly's control by forcing military decisions that the constitutional structure could not accommodate. The measurement trajectory (0.35 → 0.58 → 0.72) shows suppression intensifying: initial constitutional period had modest suppression (the structure was still holding); by month 6 suppression is nearly total (the Assembly and monarchy each suppressing what the other represented, war suppressing constitutional procedure). Theater ratio (0.58): Moderate. The constitutional ceremony was partly performative (the king's solemn oath to accept constitutional limits he did not believe in; the Assembly's faith that constitutional procedure could contain revolutionary energy) and partly functional (real establishment of representative government, real codification of rights, real reduction of royal prerogative). The trajectory is slightly downward (0.62 → 0.60 → 0.58) because the theater quality decreased as the structure's impossibility became evident — by month 6 the constitutional procedure itself was degrading toward openly coercive suspension. Not high enough for piton classification at the snare perspective, but the piton perspective (civilizational/analytical) does see the constitutional monarchy as institutional theater persisting through its own ritual.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates radical divergence across perspectives. The trapped constitutional monarchist sees pure snare — their political vision was extinguished by structural forces beyond their control. The constrained moderate sees snare from a different angle — they designed the trap themselves, discovering only through events that constitutional monarchy requires the impossible. The radical republican sees rope — the constitution was an obstacle that events removed, enabling the real coordination problem (republic vs. monarchy) to be solved. The Assembly sees tangled rope — it benefited from constitutional codification while also binding itself to an impossible compromise. The analytical observer sees either scaffold (the constitution was a provisional structure that did its job and dissolved) or piton (constitutional monarchy as institutional form persisting through theater). The civizational view of constitutional monarchy as a form, when applied to 1791, reveals piton dynamics: the form persisted through its own ceremonial performance despite its logical impossibility. The key perspectival gap: between those who see the failure as extractive (they were trapped and benefited no one) and those who see it as necessary transition (the failure was the price of clarifying what could not be clarified constitutionally).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is computed from the agent's structural position: their power level, exit options, and relationship to the extraction flow. Constitutional monarchists are trapped with no exit and no benefit — maximum directionality toward the target position (d ≈ 0.95). Moderates are constrained (high cost to exit, but possible) and positioned as discoverers of impossibility rather than pure targets — high directionality (d ≈ 0.75) but not maximal. Radicals are mobile with clear exit path (mobilization against the settlement was available) and positioned as beneficiaries — low directionality (d ≈ 0.25). The Assembly is institutional with arbitrage options (it could interpret, suspend, or abandon the constitution) and has mixed beneficiary-victim status (benefits from codification, victims to its own contradiction) — moderate directionality (d ≈ 0.50). The monarchy is institutional with arbitrage options (could have accepted constitutional limits or openly broken the settlement) and positioned as beneficiary of the chaos the settlement created — low directionality (d ≈ 0.15). The canonical d values are overridden by the structural data: beneficiaries with arbitrage options have lower d than the derivation chain would suggest because they also created the problem; victims with trapped status have higher d because the structure gave them no way to reposition.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy here is: is the 1791 Constitution best understood as a failed coordination mechanism (snare — trapped settlers extracted from by their own compromise) or as a necessary transitional structure (scaffold — a temporary support that did its job and dissolved when its time came)? The snare classification (claimed_type) reflects the perspective of settlement believers: they were trapped by the impossibility of constitutional monarchy and extracted from by the beneficiaries. But the scaffold perspective (analytical observer, generational view) suggests the constitution served essential functions before its dissolution: it crystallized revolutionary gains into law, established representative government, codified rights, and clarified the monarchy question such that it could not be avoided. The mandatrophy resolves by distinguishing temporal scales: at the biographical horizon of settlement believers, the constraint is snare (they suffer extraction). At the generational/analytical horizon, the constraint is scaffold (the constitution was a necessary temporary structure that accomplished what it could). Both perspectives are structurally true — mandatrophy is dissolved not by choosing one but by recognizing that the same structure is snare-from-the-trapped-perspective and scaffold-from-the-analytical-perspective.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    inevitability_of_failure,
    'Was the 1791 Constitution''s failure inevitable from its design, or contingent on specific events (war, Varennes, foreign pressure)?',
    'Counterfactual analysis: if the king had accepted constitutional subordination genuinely; if war had not occurred; if the flight to Varennes had been prevented. Comparison with later constitutional monarchies that succeeded. Examination of king''s actual intentions and secret correspondence.',
    'If inevitable: the constitution was a snare by design — it extracted from believers by its own impossibility. If contingent: it was a tangled rope that became a snare only under specific pressure. This distinction changes whether the settlement''s adherents were trapped by the structure or by contingent events.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(inevitability_of_failure, empirical, 'Whether constitutional monarchy failure was structurally inevitable or event-contingent').

omega_variable(
    extraction_vs_transition_cost,
    'Does the constitution''s failure represent extraction (costs imposed on settlement believers) or transition cost (necessary price of revolutionary change)?',
    'Examination of alternatives: would gradualism have sustained the settlement? Would different timing have avoided war? Were there any paths to constitutional monarchy that did not require radical mobilization to overcome?',
    'If extraction: the beneficiary (republic faction) gained political advantage from the settlement''s collapse. If transition cost: the failure was a necessary passage, not a zero-sum extraction. This affects whether the republic faction experienced the constraint as snare (they benefited from others'' entrapment) or rope (shared structural problem).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(extraction_vs_transition_cost, conceptual, 'Whether failure represents extraction or necessary revolutionary transition cost').

omega_variable(
    reading_contest_mutual_exclusion,
    'Do the failure_and_succession reading (this one) and the active_passive_citizenship reading foreclose each other, or can both describe the same constitutional text?',
    'Structural analysis: does describing the constitution as failed foreclose describing it as instantiating a two-tier citizenship? Can the citizenship structure be analyzed independently of the constitutional form''s failure?',
    'If forecloses: the readings are incompatible — either the constitution is worth analyzing for its internal structures or it is a failure to be discarded. If coexists_with: both readings are live — the citizenship division was a real structural feature of the text even though the text failed to persist. The verdict affects how to understand the 1791 kernel''s internal decomposition.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_contest_mutual_exclusion, conceptual, 'Mutual exclusion between failure_and_succession and active_passive_citizenship readings').

omega_variable(
    beneficiary_identity_in_collapse,
    'Did the Montagnard/radical republican faction actively cause the 1791 Constitution''s failure (snare extraction), or did they mobilize in response to its structural collapse from other causes?',
    'Timeline analysis: did radical action accelerate or cause failure (August insurrection, pressure for king''s deposition), or did radicals mobilize after structural failure was evident (war losses, Varennes)? Attribution of causal agency vs. responsive advantage-taking.',
    'If active: the radicals extracted advantage from the settlement by destroying it — snare classification holds from the trapped perspective. If responsive: radicals mobilized after failure became inevitable — the constraint is the structure''s own incompatibility, not extraction by another faction. This affects whether beneficiary classification is accurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_identity_in_collapse, empirical, 'Agency of radical republicans in constitutional failure vs. responsive mobilization').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(french_constitution_1791__failure_and_succession, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fc1791fs_theater_t0, french_constitution_1791__failure_and_succession, theater_ratio, 0, 0.62).
narrative_ontology:measurement(fc1791fs_theater_t3, french_constitution_1791__failure_and_succession, theater_ratio, 3, 0.6).
narrative_ontology:measurement(fc1791fs_theater_t6, french_constitution_1791__failure_and_succession, theater_ratio, 6, 0.58).

% Extraction over time
narrative_ontology:measurement(fc1791fs_extractiveness_t0, french_constitution_1791__failure_and_succession, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(fc1791fs_extractiveness_t3, french_constitution_1791__failure_and_succession, base_extractiveness, 3, 0.58).
narrative_ontology:measurement(fc1791fs_extractiveness_t6, french_constitution_1791__failure_and_succession, base_extractiveness, 6, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(fc1791fs_suppression_t0, french_constitution_1791__failure_and_succession, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(fc1791fs_suppression_t3, french_constitution_1791__failure_and_succession, suppression_requirement, 3, 0.58).
narrative_ontology:measurement(fc1791fs_suppression_t6, french_constitution_1791__failure_and_succession, suppression_requirement, 6, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(french_constitution_1791__failure_and_succession, enforcement_mechanism).
narrative_ontology:affects_constraint(french_constitution_1791__failure_and_succession, french_constitution_1791__active_passive_citizenship).
narrative_ontology:affects_constraint(french_constitution_1791__failure_and_succession, french_constitution_1791__declaration_of_rights_1789).
narrative_ontology:affects_constraint(french_constitution_1791__failure_and_succession, french_constitution_1791__suspensive_veto_monarchy).
narrative_ontology:affects_constraint(french_constitution_1791__failure_and_succession, french_constitutional_monarchy_form_legitimacy).

% DUAL FORMULATION NOTE:
% The 1791 Constitution kernel (french_constitution_1791) decomposes into four structurally distinct readings with different ε values and different beneficiary/victim structures. This story (failure_and_succession, ε=0.68) focuses on the temporal dynamic and institutional collapse. The active_passive_citizenship reading (ε≈0.55, likely tangled_rope) focuses on the citizenship division. The declaration_of_rights reading (ε≈0.35, likely rope) focuses on the universalist framing and its contradiction with particularity. The suspensive_veto reading (ε≈0.50, likely tangled_rope) focuses on the retained but subordinated monarchy. All four readings analyze the same text but extract different structural constraints from it. Network edges link all four as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(french_constitution_1791__failure_and_succession, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
