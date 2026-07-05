% ============================================================================
% CONSTRAINT STORY: marriage_commitment_legitimacy__endogenous_reinterpretation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_commitment_legitimacy__endogenous_reinterpretation_reading, []).

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
    narrative_ontology:suppression_profile/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: marriage_commitment_legitimacy__endogenous_reinterpretation_reading
 *   human_readable: 1890 Manifesto as Genuine Prophetic Revelation Preserving Church Continuity
 *   domain: religious_institutional/political_theology
 *
 * SUMMARY:
 *   This story instantiates one reading of the marriage_commitment_legitimacy
 *   kernel: that the 1890 Manifesto ending institutional sanction of new
 *   plural marriages was genuine prophetic revelation, received by the Church
 *   president as divine direction to preserve the institution for higher
 *   future purposes. Within this reading, the federal legal campaign
 *   (disincorporation, asset seizure, the Edmunds-Tucker Act) is present in
 *   the historical record as a catalyst or occasioning circumstance, but is
 *   not treated as the operative cause of the change — the change is
 *   attributed to revelation, and the reversal is read as a new stage of an
 *   unfolding covenant rather than a doctrinal defeat. This is a
 *   low-extraction reading: authority is understood to act as steward of a
 *   genuine higher-order good (institutional survival and continuing
 *   revelation) rather than as an extractive beneficiary of a coerced
 *   retreat. This story is one of three siblings under the kernel; the
 *   exogenous_override_reading and hybrid_pragmatic_reading are separate
 *   constraints with their own ε values, per the ε-invariance principle — see
 *   cs_structure.reading_relations and the omegas below for the committer
 *   structure this reading does not itself resolve.
 *
 * KEY AGENTS:
 *   - church_hierarchy: agenda_setter (institutional/analytical) — issues and administers the revelatory reframing
 *   - mainstream_membership: beneficiary (organized/constrained) — retains institutional continuity and standing
 *   - existing_plural_families: payer (powerless/trapped) — absorbs the practical cost of the reversal
 *   - fundamentalist_dissenters: payer/excluded (powerless/trapped) — reads the reversal as rupture, is excluded from the institutional interpretive process
 *   - federal_government: excluded (institutional/arbitrage) — catalytic pressure not credited as operative cause within this reading
 *   - prophetic_succession_doctrine_holders: beneficiary (institutional/analytical) — the abstract doctrine of continuing revelation is vindicated and strengthened
 *   - historians_and_outside_observers: observer (analytical/analytical) — evaluates the documentary record independent of either institutional or dissenting framing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, 0.22).
domain_priors:suppression_score(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, 0.35).
domain_priors:theater_ratio(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, extractiveness, 0.22).
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, rope).
narrative_ontology:human_readable(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, "1890 Manifesto as Genuine Prophetic Revelation Preserving Church Continuity").
narrative_ontology:topic_domain(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, "religious_institutional/political_theology").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, 'e5d94f4e-3ebe-4e0e-825b-90ab0d5f896f').
narrative_ontology:cs_kernel_codification('e5d94f4e-3ebe-4e0e-825b-90ab0d5f896f', formalized).
narrative_ontology:cs_authority_grounding('e5d94f4e-3ebe-4e0e-825b-90ab0d5f896f', lineage).
narrative_ontology:cs_interpretation_layer_present('e5d94f4e-3ebe-4e0e-825b-90ab0d5f896f').
narrative_ontology:cs_reading_relation('e5d94f4e-3ebe-4e0e-825b-90ab0d5f896f', marriage_commitment_legitimacy__exogenous_override_reading, forecloses).
narrative_ontology:cs_reading_relation('e5d94f4e-3ebe-4e0e-825b-90ab0d5f896f', marriage_commitment_legitimacy__hybrid_pragmatic_reading, coexists_with).
narrative_ontology:cs_axiom('e5d94f4e-3ebe-4e0e-825b-90ab0d5f896f', foundational, manifesto_constitutes_genuine_new_revelation).
narrative_ontology:cs_axiom_status(manifesto_constitutes_genuine_new_revelation, holdable).
narrative_ontology:cs_axiom_grounding('e5d94f4e-3ebe-4e0e-825b-90ab0d5f896f', manifesto_constitutes_genuine_new_revelation, theological).
narrative_ontology:cs_axiom('e5d94f4e-3ebe-4e0e-825b-90ab0d5f896f', secondary, federal_pressure_is_catalyst_not_operative_cause).
narrative_ontology:cs_axiom_status(federal_pressure_is_catalyst_not_operative_cause, holdable).
narrative_ontology:cs_axiom_grounding('e5d94f4e-3ebe-4e0e-825b-90ab0d5f896f', federal_pressure_is_catalyst_not_operative_cause, empirically_contingent).
narrative_ontology:cs_reference_frame('e5d94f4e-3ebe-4e0e-825b-90ab0d5f896f', plural_marriage_as_eternal_unconditional_commandment).
narrative_ontology:cs_drift_state('e5d94f4e-3ebe-4e0e-825b-90ab0d5f896f', manifesto_promulgation_1890, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('e5d94f4e-3ebe-4e0e-825b-90ab0d5f896f', '').
narrative_ontology:cs_kernel_id(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, marriage_commitment_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, church_hierarchy).
narrative_ontology:constraint_beneficiary(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, mainstream_membership).
narrative_ontology:constraint_beneficiary(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, prophetic_succession_doctrine_holders).
narrative_ontology:constraint_victim(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, existing_plural_families).
narrative_ontology:constraint_victim(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, fundamentalist_dissenters).
narrative_ontology:constraint_vindicates(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, continuing_revelation_doctrine).
narrative_ontology:constraint_vindicates(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, prophetic_authority_supremacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The Church president issues the Manifesto as a declaration received through revelation, ending the sanctioning of new plural marriages. The hierarchy administers the transition, reframes the change as continuity of divine guidance rather than capitulation, and stakes its institutional legitimacy on the claim that God directed the shift for the Church's preservation and higher purposes.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, church_hierarchy, agenda_setter,
    institutional, civilizational, analytical, national).

% Members who were not practicing plural marriage receive institutional survival, eventual statehood integration, and continuity of standing doctrine that the prophet speaks for God. Their participation in ordinary religious life is undisturbed and their faith in continuing revelation is reinforced by the reversal being framed as revelation rather than defeat.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, mainstream_membership, beneficiary,
    organized, generational, constrained, national).

% Families already formed under the prior doctrine bear the practical and social cost of the reversal: ambiguous status, loss of full institutional support, and being asked to accept that what was previously commanded as eternal and essential is now recharacterized as no longer sanctioned for the future, without their marriages being dissolved. They have little power to contest the reframing and few places to go that preserve both faith and family.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, existing_plural_families, payer,
    powerless, biographical, trapped, local).

% Those who hold that the original commandment was permanent and unconditional experience the Manifesto as a rupture in continuing revelation itself. They are read out of the mainstream body, denied a hearing within the institution's own doctrinal apparatus, and forced into separate, marginalized communities to preserve what they see as the unbroken original doctrine.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, fundamentalist_dissenters, payer,
    powerless, generational, trapped, regional).
narrative_ontology:stakeholder_secondary_role(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, fundamentalist_dissenters, excluded).

% Applied escheatment and disincorporation pressure that created the practical conditions under which the Manifesto was issued, but within this reading the federal role is catalytic rather than causal — the government's coercive apparatus is present in the historical record but is not treated as the operative explanation for the change, which this reading attributes to revelation.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, federal_government, excluded,
    institutional, biographical, arbitrage, national).

% The abstract doctrine that the Church president speaks as God's mouthpiece is vindicated and strengthened by the Manifesto's reception as revelation: a reversal on a doctrine once called eternal, absorbed without breaking prophetic authority, becomes itself evidence that continuing revelation can override prior revelation, reinforcing the institutional mechanism for all future doctrinal change.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, prophetic_succession_doctrine_holders, beneficiary,
    institutional, civilizational, analytical, universal).

% Assess the documentary and institutional record — private correspondence, timing relative to federal legal pressure, subsequent underground continuation of plural marriage sanctioned by some hierarchy members — to evaluate whether the revelatory account is the operative explanation or a legitimating narrative laid over a coerced retreat.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, historians_and_outside_observers, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a theological mechanism for the institution to change a core practice without conceding that its founding revelatory claims were wrong, preserving the coordinating function of prophetic authority across a doctrinal reversal.
% TRANSFER_FUNCTION: Moves institutional continuity and doctrinal authority from the discarded practice to the reframed one; existing plural families and fundamentalist dissenters absorb the social and familial cost of the change while the hierarchy and mainstream body retain unbroken claims to legitimate succession.
% ABSENT_VOICES: Existing plural wives and children, and fundamentalist adherents who hold the original commandment as permanent, are not treated as authoritative interpreters of the revelation within this reading; their objections are recorded historically but are not part of the institutional decision process this reading credits.
% DISAPPEARANCE_RATIONALE: If the Manifesto's status as genuine revelation were withdrawn as the operative account, the doctrine of continuing revelation through the president would need another explanation for how an eternal commandment became optional, threatening the mechanism by which the institution legitimizes all subsequent doctrinal change, including the priesthood and temple policy reversals of the twentieth century.
% FOUNDING_PROBLEM: The Church faced federal seizure of assets, disincorporation, and disenfranchisement of members tied to institutional support for plural marriage; the Manifesto was issued amid this pressure and needed to be reconciled with a standing claim that the practice was commanded by God as an eternal principle.
% FOUNDING_PROBLEM_CORROBORATION: The Church's own subsequent teaching materials and general authorities attest the Manifesto was received as revelation. Independent historians working from private diaries, correspondence between hierarchy members, and the documented timeline of federal legal pressure (escheatment proceedings, the Edmunds-Tucker Act) offer a corroborating account from outside the benefiting institution that the timing and content track legal necessity as closely as they track any independently dateable revelatory experience; this reading treats that outside account as contestable rather than dispositive.
narrative_ontology:disappearance_verdict(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, 0.22, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_commitment_legitimacy__endogenous_reinterpretation_reading_tests).
:- end_tests(marriage_commitment_legitimacy__endogenous_reinterpretation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.22 at plateau) because, within this reading, no party is understood to extract rents from a false claim — the authority structure is understood to be genuinely stewarding institutional survival for a legitimate future purpose, and the doctrinal machinery (continuing revelation through a living prophet) is treated as functioning as designed rather than as cover. Suppression (0.35) reflects the real marginalization of fundamentalist dissenters and the practical hardship imposed on existing plural families, which persists even under a genuine-revelation reading because the reversal's costs fall unevenly regardless of its theological warrant. Theater ratio rises modestly around 1890 (0.28) reflecting the intensified public presentation of the Manifesto as revelation during the period of greatest external scrutiny, then plateaus rather than escalating, consistent with a reading where the revelatory claim is treated as substantively operative rather than purely performative. Accessibility collapse is moderate-high (0.6): once the doctrine of continuing revelation is accepted as the operative frame, alternative readings of the same historical events become difficult to hold within the institution's own doctrinal commitments.
 *
 * DIRECTIONALITY LOGIC:
 *   The Church hierarchy and mainstream membership are beneficiaries under this reading — they retain unbroken institutional legitimacy and continuity of the prophetic succession doctrine, positioning them near the beneficiary end of directionality. Existing plural families and fundamentalist dissenters are payers, bearing the practical and social costs of a change from which they derive no institutional benefit and against which they have essentially no institutional recourse — they sit near the full-target end, reinforced by trapped exit options (leaving the mainstream body forecloses continued communal and family legitimacy on either side). The federal government is excluded from the reading's causal account rather than positioned as a beneficiary or victim proper, since this reading treats it as catalytic context rather than an operative party to the constraint itself.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resists mandatrophy mislabeling by treating the Manifesto's founding problem (preserving institutional survival while maintaining the coherence of continuing revelation) as still partially live rather than simply dead-and-persisting: the doctrine of prophetic succession that the Manifesto vindicates continues to be actively invoked for subsequent doctrinal changes, meaning the founding mechanism is not merely a relic being defended by inertia but an active, reused institutional capacity. Whether that capacity is itself extractive is precisely the question the sibling readings (exogenous_override, hybrid_pragmatic) contest — this reading's low ε reflects a considered position that it is not, not an artifact of ignoring the coercive context.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    revelatory_authenticity_undecidable,
    'Is the Manifesto''s reception as genuine prophetic revelation an irreducible fact about the institution''s operation, or is it a legitimating narrative constructed after the fact to reconcile a coerced policy reversal with a doctrine of institutional inerrancy?',
    'No empirical procedure can adjudicate the theological claim itself; the closest available evidence is documentary — private correspondence and diaries from the period compared against the external timeline of federal legal pressure (the Edmunds-Tucker Act, pending disincorporation and escheatment proceedings) to assess whether the revelatory account''s timing and content are independently dateable or track legal necessity too closely to separate.',
    'If the documentary record shows the revelatory claim was substantially shaped in timing and wording by legal counsel and imminent asset seizure, this reading''s low-extraction classification would be difficult to sustain and the exogenous_override_reading''s account would gain support; if independent evidence of a distinct revelatory process predating acute legal crisis is found, this reading''s claim gains support.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(revelatory_authenticity_undecidable, conceptual, 'Whether the genuine-revelation reading is a defensible independent account or an artifact of retrospective legitimation.').

omega_variable(
    beneficiary_status_of_divine_authority,
    'Does declaring ''divine authority'' and ''prophetic succession doctrine'' as beneficiaries of this constraint amount to treating a theological claim as an actor that collects rents, when in fact only human institutional actors (the hierarchy, the mainstream body) actually benefit in any observable sense?',
    'Distinguish vindicated propositions (continuing_revelation_doctrine, prophetic_authority_supremacy — which collect no rents themselves) from the human institutional actors who benefit from those propositions being sustained; this story routes the abstract doctrinal claims into vindicated_propositions and keeps prophetic_succession_doctrine_holders as a named human/institutional beneficiary group to avoid conflating the two.',
    'If the distinction collapses under scrutiny (i.e., if ''succession doctrine holders'' is functionally indistinguishable from ''the doctrine itself''), the beneficiary declaration should be revisited to ensure only real-world actors are named as beneficiaries per the schema''s naming rules.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_status_of_divine_authority, conceptual, 'Whether the beneficiary declarations correctly separate actor benefit from doctrinal vindication.').

omega_variable(
    committer_framing_underdetermination,
    'Given that the kernel supports three structurally coherent readings (endogenous revelation, exogenous coercion, hybrid pragmatic adaptation) each with different ε values and different beneficiary/victim structures, what determines which reading a given historical actor or observer adopts, and is that choice itself a function of institutional position?',
    'Cross-tabulate which reading is adopted by which class of observer (Church hierarchy statements, fundamentalist splinter group literature, secular historians, federal government records) to test whether reading choice correlates with structural position relative to the constraint (beneficiary vs. victim vs. analytical observer).',
    'If reading choice correlates strongly with structural benefit (beneficiaries consistently adopt the low-extraction endogenous reading, victims consistently adopt the high-extraction exogenous reading), that correlation itself is evidence the readings function partly as self-serving narrative rather than purely as independent historical judgments — though this does not resolve which reading is factually correct.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_framing_underdetermination, conceptual, 'Whether reading selection across the kernel tracks structural interest rather than independent evidence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, 1862, 1904).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t1862, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, theater_ratio, 1862, 0.1).
narrative_ontology:measurement(marr_tr_t1874, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, theater_ratio, 1874, 0.12).
narrative_ontology:measurement(marr_tr_t1882, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, theater_ratio, 1882, 0.15).
narrative_ontology:measurement(marr_tr_t1887, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, theater_ratio, 1887, 0.2).
narrative_ontology:measurement(marr_tr_t1890, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, theater_ratio, 1890, 0.28).
narrative_ontology:measurement(marr_tr_t1896, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, theater_ratio, 1896, 0.3).
narrative_ontology:measurement(marr_tr_t1904, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, theater_ratio, 1904, 0.28).

% Extraction over time
narrative_ontology:measurement(marr_be_t1862, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, base_extractiveness, 1862, 0.15).
narrative_ontology:measurement(marr_be_t1874, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, base_extractiveness, 1874, 0.17).
narrative_ontology:measurement(marr_be_t1882, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, base_extractiveness, 1882, 0.19).
narrative_ontology:measurement(marr_be_t1887, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, base_extractiveness, 1887, 0.2).
narrative_ontology:measurement(marr_be_t1890, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, base_extractiveness, 1890, 0.22).
narrative_ontology:measurement(marr_be_t1896, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, base_extractiveness, 1896, 0.22).
narrative_ontology:measurement(marr_be_t1904, marriage_commitment_legitimacy__endogenous_reinterpretation_reading, base_extractiveness, 1904, 0.22).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, marriage_commitment_legitimacy__exogenous_override_reading).
narrative_ontology:affects_constraint(marriage_commitment_legitimacy__endogenous_reinterpretation_reading, marriage_commitment_legitimacy__hybrid_pragmatic_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the marriage_commitment_legitimacy kernel. All three share the same underlying historical event (the 1890 Manifesto) but attribute different operative causes and different beneficiary/victim structures to it, yielding different ε values: this reading (endogenous_reinterpretation) is authored at ε≈0.22 (low extraction, genuine stewardship); the sibling exogenous_override_reading would be authored substantially higher (coerced capitulation dressed as revelation); the sibling hybrid_pragmatic_reading would be authored at an intermediate level (strategic deployment of genuine authority to manage real crisis). Per the ε-invariance principle, these are three distinct constraints, not one constraint measured three ways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
