% ============================================================================
% CONSTRAINT STORY: territorial_sovereignty_legitimacy__existential_matrix_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_territorial_sovereignty_legitimacy__existential_matrix_reading, []).

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
 *   constraint_id: territorial_sovereignty_legitimacy__existential_matrix_reading
 *   human_readable: Existential-Matrix Reading of Territorial Sovereignty Legitimacy
 *   domain: political_theory/international_relations
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested kernel
 *   territorial_sovereignty_legitimacy: the existential_matrix_reading, which
 *   holds that sovereignty legitimacy is grounded not in juridical title but
 *   in each people's existential requirement of territorial control for
 *   survival and identity, rendering the conflict structurally zero-sum and
 *   legal-historical arguments epiphenomenal. Per the epsilon-invariance
 *   principle, the sibling readings (covenant_continuity_reading,
 *   self_determination_reading) are separate constraints in separate files
 *   with their own victim sets and epsilon values; this file authors only the
 *   existential reading's constraint, whose referent is the standing
 *   arrangement under contest — the zero-sum territorial governance regime as
 *   this reading sees and reproduces it. The claim/metric gap is deliberate:
 *   the frame is CLAIMED as tangled_rope (genuine survival coordination plus
 *   heavy asymmetric extraction) while the metrics are authored from the
 *   arrangement's observed operation; the engine computes per-seat
 *   classifications and any divergence is the datum. Family decomposition
 *   note: the colloquial label 'who is legitimately sovereign here' covers
 *   three structurally distinct claims — theological-historical continuity,
 *   democratic self-determination, and existential necessity — which assign
 *   legitimacy to different sources, designate different victims, and license
 *   different concessions; forcing them into one story would make epsilon
 *   observer-dependent, which the framework forbids.
 *
 * KEY AGENTS:
 *   - maximalist_leadership_factions: agenda-setting administrator of the frame (powerful/identity_locked) — enforces the narrative it cannot psychologically exit
 *   - military_security_establishments: institutional beneficiary (institutional/constrained) — scaled to the conflict's continuation
 *   - dominant_demographic_faction: primary beneficiary seat (powerful/constrained) — the gains of territorial control accrue here
 *   - regional_powers_instrumentalizing_conflict: mobile external beneficiary (institutional/mobile) — profits from continuation at negligible cost
 *   - civilian_populations_of_both_peoples: primary target with protective offset (powerless/trapped) — pays in blood and treasure, receives solidarity and defense
 *   - occupied_and_displaced_residents: most exposed target (powerless/trapped) — governed without franchise, cited as proof of the frame
 *   - compromise_oriented_moderates: punished internal alternative (moderate/trapped, excluded seat) — the frame's principal domestic casualty
 *   - external_mediation_architecture: inter-institutional observer (institutional/analytical) — generates the legal-compromise designs the frame converts to grievance
 *   - comparative_conflict_analysts: analytical observer (analytical/analytical) — sees the full structure and the resolved-comparison baseline
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_sovereignty_legitimacy__existential_matrix_reading, 0.76).
domain_priors:suppression_score(territorial_sovereignty_legitimacy__existential_matrix_reading, 0.83).
domain_priors:theater_ratio(territorial_sovereignty_legitimacy__existential_matrix_reading, 0.46).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__existential_matrix_reading, extractiveness, 0.76).
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__existential_matrix_reading, suppression_requirement, 0.83).
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__existential_matrix_reading, theater_ratio, 0.46).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__existential_matrix_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__existential_matrix_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_sovereignty_legitimacy__existential_matrix_reading, tangled_rope).
narrative_ontology:human_readable(territorial_sovereignty_legitimacy__existential_matrix_reading, "Existential-Matrix Reading of Territorial Sovereignty Legitimacy").
narrative_ontology:topic_domain(territorial_sovereignty_legitimacy__existential_matrix_reading, "political_theory/international_relations").

domain_priors:requires_active_enforcement(territorial_sovereignty_legitimacy__existential_matrix_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_sovereignty_legitimacy__existential_matrix_reading, '3213b991-cdc7-46c4-8d7c-ad2a570e39c8').
narrative_ontology:cs_kernel_codification('3213b991-cdc7-46c4-8d7c-ad2a570e39c8', distributed).
narrative_ontology:cs_authority_grounding('3213b991-cdc7-46c4-8d7c-ad2a570e39c8', practice).
narrative_ontology:cs_interpretation_layer_present('3213b991-cdc7-46c4-8d7c-ad2a570e39c8').
narrative_ontology:cs_reading_relation('3213b991-cdc7-46c4-8d7c-ad2a570e39c8', territorial_sovereignty_legitimacy__covenant_continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('3213b991-cdc7-46c4-8d7c-ad2a570e39c8', territorial_sovereignty_legitimacy__self_determination_reading, forecloses).
narrative_ontology:cs_axiom('3213b991-cdc7-46c4-8d7c-ad2a570e39c8', foundational, territorial_control_precondition_for_collective_survival).
narrative_ontology:cs_axiom_status(territorial_control_precondition_for_collective_survival, holdable).
narrative_ontology:cs_axiom_grounding('3213b991-cdc7-46c4-8d7c-ad2a570e39c8', territorial_control_precondition_for_collective_survival, empirically_contingent).
narrative_ontology:cs_axiom('3213b991-cdc7-46c4-8d7c-ad2a570e39c8', foundational, dyadic_rival_claim_conflict_is_zero_sum).
narrative_ontology:cs_axiom_status(dyadic_rival_claim_conflict_is_zero_sum, holdable).
narrative_ontology:cs_axiom_grounding('3213b991-cdc7-46c4-8d7c-ad2a570e39c8', dyadic_rival_claim_conflict_is_zero_sum, empirically_contingent).
narrative_ontology:cs_axiom('3213b991-cdc7-46c4-8d7c-ad2a570e39c8', secondary, juridical_arguments_epiphenomenal_to_outcomes).
narrative_ontology:cs_axiom_status(juridical_arguments_epiphenomenal_to_outcomes, holdable).
narrative_ontology:cs_axiom_grounding('3213b991-cdc7-46c4-8d7c-ad2a570e39c8', juridical_arguments_epiphenomenal_to_outcomes, empirically_contingent).
narrative_ontology:cs_reference_frame('3213b991-cdc7-46c4-8d7c-ad2a570e39c8', territorial_control_as_existential_precondition).
narrative_ontology:cs_drift_state('3213b991-cdc7-46c4-8d7c-ad2a570e39c8', contemporary_post_opening_collapse_era, gap(revival_pressure, minor, true)).
narrative_ontology:cs_created_at('3213b991-cdc7-46c4-8d7c-ad2a570e39c8', '').
narrative_ontology:cs_kernel_id(territorial_sovereignty_legitimacy__existential_matrix_reading, territorial_sovereignty_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(territorial_sovereignty_legitimacy__existential_matrix_reading, maximalist_leadership_factions).
narrative_ontology:constraint_beneficiary(territorial_sovereignty_legitimacy__existential_matrix_reading, military_security_establishments).
narrative_ontology:constraint_beneficiary(territorial_sovereignty_legitimacy__existential_matrix_reading, dominant_demographic_faction).
narrative_ontology:constraint_beneficiary(territorial_sovereignty_legitimacy__existential_matrix_reading, regional_powers_instrumentalizing_conflict).
narrative_ontology:constraint_victim(territorial_sovereignty_legitimacy__existential_matrix_reading, civilian_populations_of_both_peoples).
narrative_ontology:constraint_victim(territorial_sovereignty_legitimacy__existential_matrix_reading, occupied_and_displaced_residents).
narrative_ontology:constraint_victim(territorial_sovereignty_legitimacy__existential_matrix_reading, compromise_oriented_moderates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(territorial_sovereignty_legitimacy__existential_matrix_reading, civilian_populations_of_both_peoples).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Political leaderships within each people whose domestic standing rests on insisting the rival's claims can never be accommodated. They shape school curricula, set security doctrine, decide which negotiations occur and which are taboo, and define what counts as betrayal. Their careers were made inside the existential narrative; stepping outside it would end them politically, so they maintain the frame they administer.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__existential_matrix_reading, maximalist_leadership_factions, agenda_setter,
    powerful, biographical, identity_locked, national).

% Standing armies, intelligence services, and defense industries on both sides. They receive budgets, emergency legal powers, and social centrality proportional to how permanent the threat is understood to be. Individual officers sincerely hold the threat assessments; the institution as such is sized, staffed, and budgeted for the conflict's continuation rather than its resolution.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__existential_matrix_reading, military_security_establishments, beneficiary,
    institutional, generational, constrained, national).

% Whichever community currently converts territorial control into durable advantage through settlement growth, military preponderance, or demographic engineering. Its members' property, physical safety, and political weight are bound up in the territory remaining divided on terms favorable to them, and their position improves with every year the zero-sum structure holds.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__existential_matrix_reading, dominant_demographic_faction, beneficiary,
    powerful, generational, constrained, regional).

% Neighboring states and extra-regional patrons that fund, arm, or diplomatically shield one side or the other. The unresolved conflict supplies them with distraction from domestic failures, leverage over rivals, and justification for their own security spending. They can scale involvement up or down at will; the conflict's continuation costs them comparatively little.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__existential_matrix_reading, regional_powers_instrumentalizing_conflict, beneficiary,
    institutional, generational, mobile, continental).

% Ordinary families on both sides. They supply the soldiers, absorb the casualties, live under rocket alerts or checkpoint regimes, pay for the war economy through taxation and forgone trade, and bury the dead. The same arrangement that costs them also delivers protection, solidarity, and belonging; emigration remains available to those with means and is widely experienced as a further loss rather than an escape.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__existential_matrix_reading, civilian_populations_of_both_peoples, payer,
    powerless, generational, trapped, regional).
narrative_ontology:stakeholder_secondary_role(territorial_sovereignty_legitimacy__existential_matrix_reading, civilian_populations_of_both_peoples, beneficiary).

% Residents living under the control arrangements without citizenship rights in the governing power: checkpointed, administratively detained, subject to home demolition or exile, and unable to vote out the authority that rules them. Their community's earlier displacement is invoked by their own leadership as proof that vulnerability is fatal, binding them more tightly to the governing narrative.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__existential_matrix_reading, occupied_and_displaced_residents, payer,
    powerless, generational, trapped, local).

% Politicians, clergy, veterans, and civic organizers on both sides who argue that mutual recognition is achievable. They lose elections, face primary challenges and defamation campaigns, are branded traitors, and in the extreme case are assassinated. Every collapse of a peace process strengthens their opponents and narrows the space they occupy.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__existential_matrix_reading, compromise_oriented_moderates, payer,
    moderate, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(territorial_sovereignty_legitimacy__existential_matrix_reading, compromise_oriented_moderates, excluded).

% International organizations, great-power envoys, and NGO frameworks that draft partition plans, recognition sequences, and interim arrangements. They see both sides' red lines from outside, continue producing legal-compromise designs, and record — proposal after proposal — how each plan is converted into fresh grievance by the parties' governing assumptions.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__existential_matrix_reading, external_mediation_architecture, observer,
    institutional, generational, analytical, global).

% Scholars of ethnic conflict and security dilemmas who compare this dyad with rival-claim cases that resolved. They document which compromises held elsewhere, estimate how much of the threat perception tracks material capability versus inherited narrative, and publish findings that neither leadership adopts.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__existential_matrix_reading, comparative_conflict_analysts, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(territorial_sovereignty_legitimacy__existential_matrix_reading, dominant_demographic_faction).
narrative_ontology:fixing_cost_class(territorial_sovereignty_legitimacy__existential_matrix_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates each people's collective response to a real security dilemma: a shared threat perception, mobilization and sacrifice discipline, and boundary maintenance under conditions where the rival people's parallel claim to the same territory makes unilateral vulnerability potentially catastrophic.
% TRANSFER_FUNCTION: Moves lives (conscription and casualties), economic output (defense spending and severed trade), civil liberties (emergency powers applied to each side's own population), and generational attention away from ordinary members of both societies and toward the factions and institutions that administer the existential narrative; it also moves territorial control incrementally toward whichever side accumulates coercive and demographic advantage.
% ABSENT_VOICES: Compromise-oriented moderates on both sides, advocates of binational or civic-identity arrangements, and stateless residents of the territory are systematically outside the negotiating frame; so is the next generation, which inherits the arrangement without having consented to it. They sit in opposition benches, in banned or marginal movements, or outside the polity's franchise entirely, and enter summit rooms only as witnesses to decisions already taken.
% DISAPPEARANCE_RATIONALE: If the existential-matrix governance vanished overnight, demobilization would begin within months, ruling coalitions built on maximalism would collapse, defense economies would contract, and every legitimacy claim in the territory would have to be renegotiated on non-existential terms. The underlying rival claims and historical grievances would persist, but the entire apparatus of permanent mobilization, securitized daily life, and compromise-foreclosure is organized around this frame and would lose its organizing principle.
% FOUNDING_PROBLEM: How does a people guarantee collective survival when the rival claimant's total victory could mean annihilation or permanent dispossession? The frame was consolidated out of catastrophic trauma on both sides — for one people, industrialized genocide establishing that statelessness can be lethal; for the other, mass displacement establishing that defeat means losing home, land, and return forever.
% FOUNDING_PROBLEM_CORROBORATION: Historical scholarship outside all benefiting parties corroborates the founding traumas themselves (Holocaust historiography; displacement and refugee documentation), and security-studies literature independently establishes that the dyadic security dilemma is empirically real rather than invented. Large-N survey research showing that majorities on both sides rank physical security above territorial maximality corroborates the problem's liveness while contesting the frame's monopoly on answering it — the problem is attested; the frame's claim to be the only possible response is not.
narrative_ontology:disappearance_verdict(territorial_sovereignty_legitimacy__existential_matrix_reading, world_rearranges).
narrative_ontology:founding_problem_status(territorial_sovereignty_legitimacy__existential_matrix_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(territorial_sovereignty_legitimacy__existential_matrix_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(territorial_sovereignty_legitimacy__existential_matrix_reading, 'none', 1).
narrative_ontology:epsilon_provenance(territorial_sovereignty_legitimacy__existential_matrix_reading, 0.76, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(territorial_sovereignty_legitimacy__existential_matrix_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(territorial_sovereignty_legitimacy__existential_matrix_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(territorial_sovereignty_legitimacy__existential_matrix_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is high (0.76) because the arrangement continuously converts both societies' lives, labor, liberties, and generational horizons into the dominant faction's positional advantage, and the rate of conversion is decoupled from any defensive necessity that diminishes over time. Suppression is higher still (0.83) because the frame's persistence depends on actively closing exits: extremist violence punishes visible vulnerability, contact and cooperation are legally or socially penalized, and each side's education and memorial systems reproduce the threat perception before any rival evidence arrives. Theater is moderate (0.46) and rising: core defense functions are real, but a growing share of activity — resolve-signaling, commemorative mobilization, performative rejection of plans already dead — maintains the narrative rather than the population. Accessibility collapse is 0.70: inside the frame, compromise alternatives collapse almost completely (they become unthinkable or treasonous), yet the frame never fully closes the conceptual space — sibling readings and moderation persist at the margins, which is why resistance holds at 0.60 rather than collapsing. The measurement series run on one shared time grid (every tracked metric authored at every examined year) so no end-state value is silently substituted into earlier rows. The series are deliberately cyclical rather than monotonic: each escalation cycle (1948, 1967, 1973, 1987, 2000) spikes extraction and suppression; each diplomatic opening (most visibly 1993) relaxes all three; each collapse of an opening restores and exceeds the prior peak. The oscillation is not noise — it is intermittent reinforcement: every cycle's trauma re-fuses identity, discredits the moderates who advocated the opening, and raises the next baseline, which is why the envelope trends upward across cycles. The rising base_extractiveness trajectory is expected to trip the T17 abductive trigger (mountain_extraction_accumulation); that is intended — the accumulation is the historical record, and T17's hypothesis (that a constraint presenting as natural necessity is accumulating rent) is precisely the live question here.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat and the payer seats compute categorically different types from identical structural data. From inside the maximalist leadership, the arrangement is survival coordination it administers under mortal constraint — every cost is imposed by the rival's existence, no one is extracting, and compromise advocates are objectively dangerous. From the civilian and occupied seats, the same structure operates as permanent levying of blood, money, and freedom for the benefit of factions and institutions that never bear the costs proportionally. The two peoples' civilian seats diverge despite comparable nominal standing: one electorate can rotate its leadership and punish maximalism at the ballot box, the other cannot vote out the authority that governs it at all — same power atom, radically different exit texture, which is why their effective positions differ. Cross-side civilian coalition (joint civic movements, bereaved-parent networks) is the classic coalition-power route for targets, and it repeatedly forms and is repeatedly crushed — asymmetrically, because each side's moderates are punished by their own camp harder than by the enemy, which is the signature of internalized rather than purely structural suppression.
 *
 * DIRECTIONALITY LOGIC:
 *   Declarations map to directionality as follows. The dominant_demographic_faction sits nearest the beneficiary pole: it collects the arrangement's positional gains directly and bears its costs least. Maximalist_leadership_factions derive low d from their beneficiary listing, but their identity_locked exit means they cannot arbitrage away even if they wished — they are locked INTO the beneficiary position, which stabilizes their low d rather than raising it. Military_security_establishments collect budgets and status without setting the frame — low d, constrained. Regional powers are beneficiaries with mobile exit: they capture option value from the conflict's continuation while retaining the ability to disengage, placing them near the beneficiary pole with the least commitment. Civilian_populations carry a dual declaration (payer with beneficiary secondary role): the derivation reads their victim status and trapped exit toward the target pole, while the protective-coordination benefit pulls the other way — their honest position is mid-to-high d, paying far more than they receive but receiving something real. Occupied_and_displaced_residents take the highest d in the story: full-cost bearers with no franchise, no protective reciprocity, and no exit. Compromise_oriented_moderates are high-d targets of a distinctive kind — they pay politically and sometimes physically for advocating the alternatives the frame suppresses. Observers hold analytical seats and feed no extraction arithmetic.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope claim is what prevents misclassification in both directions. Calling this a rope would erase the extraction: the coordination function (survival mobilization under a real security dilemma) is genuine, but it is fused to a transfer machine that moves lives and wealth to dominant factions and forecloses every exit. Calling it a snare would erase the dilemma: unlike a pure snare, the fear is not fabricated — the founding traumas are documented, the rival maximal claims are real, and a people that unilaterally disarmed the frame's protections would face genuine catastrophic risk. The frame is emphatically not a piton: its function has not atrophied, it is vigorously maintained, and its administrators profit — the concentrated-beneficiary test rules the piton cell out. On the genealogy interview, founding_problem_status is live and disappearance_verdict is world_rearranges, so the mismatch consumer finds no dead-mandate/zombie flag: the problem the frame was built for still exists. The mandatrophy-relevant signal is subtler and lives in the temporal data — the post-2000 divergence between rising theater_ratio and unchanged objective threat indicators suggests partial capture: the protective function is increasingly performed while the transfer function compounds, which is the early morphology of a coordination structure drifting toward its extraction component without yet having abandoned the coordination it genuinely performs.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_selection_uncertainty,
    'This constraint is one reading of the kernel territorial_sovereignty_legitimacy (existential_matrix_reading). Which reading correctly identifies the operative driver of the conflict''s persistence — existential necessity (this reading), theological-historical covenant continuity (covenant_continuity_reading), or democratic self-determination (self_determination_reading)?',
    'Comparative outcome analysis across the three readings'' distinct predictions: the existential reading predicts legal settlements fail regardless of their terms; the covenant reading predicts outcomes track recognition of historical-divine title; the self-determination reading predicts outcomes track enfranchisement of the demographic majority. Longitudinal coding of negotiation failures against which variable each failure turned on.',
    'Adopting the self-determination reading relocates the victim set to the population denied majority-rule expression and reclassifies the arrangement around disenfranchisement; adopting the covenant reading relocates legitimacy to theological-historical continuity and changes which concessions are even articulable; retaining this reading keeps both peoples'' civilians as symmetric cost-bearers. Epsilon, victim sets, and classification all shift with the selection.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_uncertainty, conceptual, 'Which reading of the territorial-sovereignty kernel is structurally operative.').

omega_variable(
    disagreement_location_grounding_axis,
    'Where exactly do the three readings disagree? The located difference is the GROUNDING axis of legitimacy — theological-historical continuity versus existential necessity versus democratic will — and, downstream of it, which concessions each reading renders thinkable (territorial partition, repatriation, power-sharing).',
    'Not resolvable by evidence alone: it is a conceptual dispute over what kind of fact legitimacy is. Partial resolution by tracking which grounding each party''s actual behavior reveals when the groundings conflict (e.g., a party accepting unfavorable legal terms but refusing existentially framed ones exposes its operative ground).',
    'If the operative groundings differ across parties (likely), no single-reading classification of the dyad is available and the three stories must be evaluated as a linked family; if one grounding dominates behavior, the corresponding reading''s constraint carries the classification and the others become epiphenomenal.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(disagreement_location_grounding_axis, conceptual, 'The specific structural element (legitimacy grounding) on which sibling readings diverge.').

omega_variable(
    objective_dilemma_vs_frame_amplification,
    'How much of the measured zero-sum structure is an objective security dilemma between two peoples with rival maximal claims, and how much is amplified and maintained by the existential frame itself?',
    'Counterfactual analysis of diplomatic-opening windows (principally the 1993-2000 period): did objective threat indicators (attack rates, armament trajectories, border incidents) fall faster than the frame permitted acknowledgment, and did material conditions improve while subjective threat held constant? Comparative baseline against resolved rival-claim dyads with similar initial endowments.',
    'If the dilemma is largely objective, a substantial fraction of the measured cost is the irreducible price of survival coordination and the constraint sits nearer the rope end of the tangled spectrum. If largely frame-amplified, the coordination story is substantially cover, extraction is higher than authored, and the constraint migrates toward the snare boundary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(objective_dilemma_vs_frame_amplification, empirical, 'Objective security dilemma versus frame-manufactured zero-sum structure.').

omega_variable(
    beneficiary_seat_mobility_over_time,
    'The beneficiary seat is defined positionally — whichever side achieves demographic/military dominance — so the seat''s occupant changes across the interval. Does the classification track the seat or the structure, and does occupant turnover alter epsilon?',
    'Re-run classification at successive dominance equilibria (pre-1967, post-1967, post-2000 settlement expansion) holding the structural declarations fixed; observe whether computed per-seat types are invariant under occupant turnover.',
    'If invariant, the constraint is a stable structure with rotating occupants and the story needs no temporal re-authoring; if variant, the story requires period-split decomposition with separate epsilon per dominance regime, since a moving beneficiary seat makes single-interval epsilon unstable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_seat_mobility_over_time, empirical, 'Whether the positionally-defined beneficiary seat destabilizes single-interval classification.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the measured suppression of compromise alternatives primarily structural (extremist violence punishing vulnerability, legal penalties on cooperation, checkpoint geography) or internalized (generations educated into identity fusion where concession equals betrayal, memorial cultures that render empathy with the rival''s claim psychologically unavailable)?',
    'Post-opening suppression trajectory: during and after diplomatic openings, if compromise-support rebounds quickly once formal barriers lift, suppression is mostly structural; if support stays depressed across generations despite barrier removal, the residue is internalized. Survey time-series on compromise support across cohort and exposure-to-cooperation variables.',
    'If predominantly internalized, the constraint''s effective suppression exceeds the structural measure — targets carry the frame across borders and into emigration, exits that look open are not, and effective extraction rises accordingly. If predominantly structural, removing the enforcement machinery would release suppressed alternatives rapidly and the snare-ward migration risk is lower.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Structural versus internalized mechanism carrying the frame''s suppression load.').

omega_variable(
    existential_requirement_naturalness,
    'Is ''each people requires territorial control as precondition for collective survival and identity expression'' an anthropological constant of collective life, or a historically contingent doctrine produced by specific twentieth-century traumas and maintained by specific institutions?',
    'Cross-case comparison: catalog peoples that preserved identity and survived existential threat without territorial sovereignty (diaspora nations, stateless nations under protection regimes) against the frame''s prediction that they should have dissolved; test whether the requirement''s apparent universality survives cases where sovereignty was absent but survival succeeded.',
    'If the requirement is contingent, the frame''s natural-law presentation is a false summit and identifiable beneficiaries are maintaining a constructed constraint dressed as nature — pushing classification toward the extractive end and validating the vindicated_propositions list as the frame''s intellectual subsidy. If robustly constant, part of the arrangement''s cost is genuinely irreducible and the coordination floor absorbs more of the measured extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(existential_requirement_naturalness, conceptual, 'Natural law versus constructed doctrine status of the existential-survival premise.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_sovereignty_legitimacy__existential_matrix_reading, 1948, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tsl_existential_tr_t1948, territorial_sovereignty_legitimacy__existential_matrix_reading, theater_ratio, 1948, 0.2).
narrative_ontology:measurement(tsl_existential_tr_t1956, territorial_sovereignty_legitimacy__existential_matrix_reading, theater_ratio, 1956, 0.23).
narrative_ontology:measurement(tsl_existential_tr_t1967, territorial_sovereignty_legitimacy__existential_matrix_reading, theater_ratio, 1967, 0.28).
narrative_ontology:measurement(tsl_existential_tr_t1973, territorial_sovereignty_legitimacy__existential_matrix_reading, theater_ratio, 1973, 0.3).
narrative_ontology:measurement(tsl_existential_tr_t1987, territorial_sovereignty_legitimacy__existential_matrix_reading, theater_ratio, 1987, 0.34).
narrative_ontology:measurement(tsl_existential_tr_t1993, territorial_sovereignty_legitimacy__existential_matrix_reading, theater_ratio, 1993, 0.26).
narrative_ontology:measurement(tsl_existential_tr_t2000, territorial_sovereignty_legitimacy__existential_matrix_reading, theater_ratio, 2000, 0.39).
narrative_ontology:measurement(tsl_existential_tr_t2005, territorial_sovereignty_legitimacy__existential_matrix_reading, theater_ratio, 2005, 0.43).
narrative_ontology:measurement(tsl_existential_tr_t2018, territorial_sovereignty_legitimacy__existential_matrix_reading, theater_ratio, 2018, 0.45).
narrative_ontology:measurement(tsl_existential_tr_t2026, territorial_sovereignty_legitimacy__existential_matrix_reading, theater_ratio, 2026, 0.46).

% Extraction over time
narrative_ontology:measurement(tsl_existential_be_t1948, territorial_sovereignty_legitimacy__existential_matrix_reading, base_extractiveness, 1948, 0.66).
narrative_ontology:measurement(tsl_existential_be_t1956, territorial_sovereignty_legitimacy__existential_matrix_reading, base_extractiveness, 1956, 0.63).
narrative_ontology:measurement(tsl_existential_be_t1967, territorial_sovereignty_legitimacy__existential_matrix_reading, base_extractiveness, 1967, 0.7).
narrative_ontology:measurement(tsl_existential_be_t1973, territorial_sovereignty_legitimacy__existential_matrix_reading, base_extractiveness, 1973, 0.67).
narrative_ontology:measurement(tsl_existential_be_t1987, territorial_sovereignty_legitimacy__existential_matrix_reading, base_extractiveness, 1987, 0.71).
narrative_ontology:measurement(tsl_existential_be_t1993, territorial_sovereignty_legitimacy__existential_matrix_reading, base_extractiveness, 1993, 0.57).
narrative_ontology:measurement(tsl_existential_be_t2000, territorial_sovereignty_legitimacy__existential_matrix_reading, base_extractiveness, 2000, 0.73).
narrative_ontology:measurement(tsl_existential_be_t2005, territorial_sovereignty_legitimacy__existential_matrix_reading, base_extractiveness, 2005, 0.71).
narrative_ontology:measurement(tsl_existential_be_t2018, territorial_sovereignty_legitimacy__existential_matrix_reading, base_extractiveness, 2018, 0.75).
narrative_ontology:measurement(tsl_existential_be_t2026, territorial_sovereignty_legitimacy__existential_matrix_reading, base_extractiveness, 2026, 0.76).

% Suppression requirement over time
narrative_ontology:measurement(tsl_existential_su_t1948, territorial_sovereignty_legitimacy__existential_matrix_reading, suppression_requirement, 1948, 0.55).
narrative_ontology:measurement(tsl_existential_su_t1956, territorial_sovereignty_legitimacy__existential_matrix_reading, suppression_requirement, 1956, 0.58).
narrative_ontology:measurement(tsl_existential_su_t1967, territorial_sovereignty_legitimacy__existential_matrix_reading, suppression_requirement, 1967, 0.66).
narrative_ontology:measurement(tsl_existential_su_t1973, territorial_sovereignty_legitimacy__existential_matrix_reading, suppression_requirement, 1973, 0.63).
narrative_ontology:measurement(tsl_existential_su_t1987, territorial_sovereignty_legitimacy__existential_matrix_reading, suppression_requirement, 1987, 0.71).
narrative_ontology:measurement(tsl_existential_su_t1993, territorial_sovereignty_legitimacy__existential_matrix_reading, suppression_requirement, 1993, 0.59).
narrative_ontology:measurement(tsl_existential_su_t2000, territorial_sovereignty_legitimacy__existential_matrix_reading, suppression_requirement, 2000, 0.76).
narrative_ontology:measurement(tsl_existential_su_t2005, territorial_sovereignty_legitimacy__existential_matrix_reading, suppression_requirement, 2005, 0.79).
narrative_ontology:measurement(tsl_existential_su_t2018, territorial_sovereignty_legitimacy__existential_matrix_reading, suppression_requirement, 2018, 0.82).
narrative_ontology:measurement(tsl_existential_su_t2026, territorial_sovereignty_legitimacy__existential_matrix_reading, suppression_requirement, 2026, 0.83).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(territorial_sovereignty_legitimacy__existential_matrix_reading, identity_coordination).
narrative_ontology:affects_constraint(territorial_sovereignty_legitimacy__existential_matrix_reading, territorial_sovereignty_legitimacy__covenant_continuity_reading).
narrative_ontology:affects_constraint(territorial_sovereignty_legitimacy__existential_matrix_reading, territorial_sovereignty_legitimacy__self_determination_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'territorial sovereignty legitimacy' decomposes into three structurally distinct constraints sharing one kernel. covenant_continuity_reading (grounding: theological-historical continuity; victims: those outside the covenant claim's protection) and self_determination_reading (grounding: democratic will of the modern-period demographic majority; victims: the population denied majority-rule expression) both instantiate legitimacy criteria this reading declares epiphenomenal. The existential reading sits DOWNSTREAM of both as their cited executioner — its central empirical claim ('legal settlements fail regardless of terms') is deployed against both siblings' compromise outputs, so this story links to both as structural influence. Epsilon differs sharply across the family: the siblings authorize compromise architectures (partition, repatriation, recognition sequences) whose failure this reading predicts and feeds on; this reading's epsilon reflects the standing zero-sum arrangement's continuous conversion of both societies' resources into positional advantage. Each file documents the decomposition; no story attempts to average across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
