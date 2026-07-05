% ============================================================================
% CONSTRAINT STORY: imperial_mandate__loyalist_restoration_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_imperial_mandate__loyalist_restoration_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: imperial_mandate__loyalist_restoration_reading
 *   human_readable: Loyalist Restoration Reading of Imperial Divine Mandate (Unmediated Sovereignty)
 *   domain: political philosophy / comparative constitutional systems / East Asian history
 *
 * SUMMARY:
 *   This constraint models the loyalist restoration reading of the imperial
 *   mandate kernel: the claim that divine legitimacy requires the emperor to
 *   personally and unmediatedly exercise sovereignty, such that any
 *   intermediary governing structure — above all the Tokugawa shogunate and
 *   the hereditary samurai order that sustained it — is not a valid
 *   delegation but a usurpation requiring correction. This reading was the
 *   doctrinal engine of the Meiji Restoration: it converted a factional and
 *   military conflict into a claim about restoring a corrupted natural order,
 *   licensing the abolition of the shogunate, the domains, and eventually the
 *   samurai class itself. The sibling reading (bakufu_delegation_reading,
 *   generated separately) holds that the mandate's legitimacy-granting
 *   function is separable from active governance and can be validly delegated
 *   — under which the shogunate is not usurpation but proper stewardship. The
 *   two readings are structurally incompatible on the specific question of
 *   whether delegation counts as fidelity to or violation of the mandate;
 *   they are NOT the same constraint measured two ways — each has its own
 *   beneficiary/victim structure, its own enforcement history, and its own
 *   epsilon.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(imperial_mandate__loyalist_restoration_reading, 0.58).
domain_priors:suppression_score(imperial_mandate__loyalist_restoration_reading, 0.71).
domain_priors:theater_ratio(imperial_mandate__loyalist_restoration_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(imperial_mandate__loyalist_restoration_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(imperial_mandate__loyalist_restoration_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(imperial_mandate__loyalist_restoration_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(imperial_mandate__loyalist_restoration_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(imperial_mandate__loyalist_restoration_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(imperial_mandate__loyalist_restoration_reading, tangled_rope).
narrative_ontology:human_readable(imperial_mandate__loyalist_restoration_reading, "Loyalist Restoration Reading of Imperial Divine Mandate (Unmediated Sovereignty)").
narrative_ontology:topic_domain(imperial_mandate__loyalist_restoration_reading, "political philosophy / comparative constitutional systems / East Asian history").

domain_priors:requires_active_enforcement(imperial_mandate__loyalist_restoration_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(imperial_mandate__loyalist_restoration_reading, '6324ee8b-a255-40d0-a33f-c061798b30b3').
narrative_ontology:cs_kernel_codification('6324ee8b-a255-40d0-a33f-c061798b30b3', distributed).
narrative_ontology:cs_authority_grounding('6324ee8b-a255-40d0-a33f-c061798b30b3', lineage).
narrative_ontology:cs_interpretation_layer_present('6324ee8b-a255-40d0-a33f-c061798b30b3').
narrative_ontology:cs_reading_relation('6324ee8b-a255-40d0-a33f-c061798b30b3', imperial_mandate__bakufu_delegation_reading, forecloses).
narrative_ontology:cs_axiom('6324ee8b-a255-40d0-a33f-c061798b30b3', foundational, sovereignty_and_governance_are_inseparable).
narrative_ontology:cs_axiom_status(sovereignty_and_governance_are_inseparable, holdable).
narrative_ontology:cs_axiom_grounding('6324ee8b-a255-40d0-a33f-c061798b30b3', sovereignty_and_governance_are_inseparable, deontological).
narrative_ontology:cs_axiom('6324ee8b-a255-40d0-a33f-c061798b30b3', foundational, intermediary_rule_constitutes_usurpation).
narrative_ontology:cs_axiom_status(intermediary_rule_constitutes_usurpation, holdable).
narrative_ontology:cs_axiom_grounding('6324ee8b-a255-40d0-a33f-c061798b30b3', intermediary_rule_constitutes_usurpation, conventional).
narrative_ontology:cs_axiom('6324ee8b-a255-40d0-a33f-c061798b30b3', secondary, institutional_rupture_required_for_legitimate_restoration).
narrative_ontology:cs_axiom_status(institutional_rupture_required_for_legitimate_restoration, overridden).
narrative_ontology:cs_axiom_grounding('6324ee8b-a255-40d0-a33f-c061798b30b3', institutional_rupture_required_for_legitimate_restoration, instrumental).
narrative_ontology:cs_reference_frame('6324ee8b-a255-40d0-a33f-c061798b30b3', pre_tokugawa_direct_imperial_rule).
narrative_ontology:cs_drift_state('6324ee8b-a255-40d0-a33f-c061798b30b3', meiji_constitutional_settlement_1889, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('6324ee8b-a255-40d0-a33f-c061798b30b3', '').
narrative_ontology:cs_kernel_id(imperial_mandate__loyalist_restoration_reading, imperial_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(imperial_mandate__loyalist_restoration_reading, imperial_court_loyalists).
narrative_ontology:constraint_beneficiary(imperial_mandate__loyalist_restoration_reading, restoration_domain_factions).
narrative_ontology:constraint_beneficiary(imperial_mandate__loyalist_restoration_reading, emperor_meiji_and_household).
narrative_ontology:constraint_victim(imperial_mandate__loyalist_restoration_reading, tokugawa_bakufu_officials).
narrative_ontology:constraint_victim(imperial_mandate__loyalist_restoration_reading, hereditary_samurai_retainers).
narrative_ontology:constraint_victim(imperial_mandate__loyalist_restoration_reading, regional_daimyo_administrations).
narrative_ontology:constraint_vindicates(imperial_mandate__loyalist_restoration_reading, unmediated_imperial_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(imperial_mandate__loyalist_restoration_reading, restoration_as_correction_of_usurpation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Court nobles and allied domain samurai who construct and enforce the reading that legitimacy requires the emperor to personally exercise administrative sovereignty. They administer the restoration project, write the founding proclamations, and mobilize military and ideological force against the shogunate on the strength of this reading. Their own status rises directly with the reading's success.
narrative_ontology:constraint_stakeholder(imperial_mandate__loyalist_restoration_reading, imperial_court_loyalists, agenda_setter,
    organized, generational, identity_locked, national).

% Satsuma, Choshu, and allied domains that back the unmediated-sovereignty reading militarily and politically, converting their support into new positions inside the reorganized imperial state once the shogunate is dismantled. They gain administrative power precisely because the reading delegitimizes the intermediary structure they are displacing.
narrative_ontology:constraint_stakeholder(imperial_mandate__loyalist_restoration_reading, restoration_domain_factions, beneficiary,
    powerful, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(imperial_mandate__loyalist_restoration_reading, restoration_domain_factions, agenda_setter).

% The imperial person and household are elevated from ritual figurehead to claimed active administrative sovereign. The emperor's own room to maneuver is not fully free — the reading obligates continuous visible governance activity, foreign-policy initiative, and personal identification with state decisions in a way ritual sovereignty never required.
narrative_ontology:constraint_stakeholder(imperial_mandate__loyalist_restoration_reading, emperor_meiji_and_household, beneficiary,
    institutional, civilizational, identity_locked, national).

% Shogunal administrators whose entire governing authority is retroactively redefined as usurpation under this reading. They have no path to retain office or legitimacy once the unmediated-sovereignty premise is asserted; their options collapse to surrender, resistance (Boshin War), or exile.
narrative_ontology:constraint_stakeholder(imperial_mandate__loyalist_restoration_reading, tokugawa_bakufu_officials, payer,
    powerful, biographical, trapped, national).

% Retainers whose status, stipends, and social position were embedded in the shogunate-samurai order. Under this reading their entire class structure is delegitimized as an obstacle to direct imperial rule, and many face abolition of stipends, loss of caste privilege, and forced reintegration into a new administrative or military order not of their choosing.
narrative_ontology:constraint_stakeholder(imperial_mandate__loyalist_restoration_reading, hereditary_samurai_retainers, payer,
    moderate, biographical, constrained, regional).

% Domain lords who administered semi-autonomous territories under bakufu sanction. This reading requires the eventual dissolution of domain autonomy (haihan-chiken) as a logical consequence of unmediated imperial sovereignty, converting former rulers into salaried peers with no independent governing base.
narrative_ontology:constraint_stakeholder(imperial_mandate__loyalist_restoration_reading, regional_daimyo_administrations, payer,
    powerful, biographical, constrained, regional).

% The sibling reading of the same kernel, under which the emperor's legitimacy-granting function is separable from active governance and can be validly delegated to a shogunate. This reading is not a party but the foreclosed alternative framework this constraint's proponents must argue against; its adherents (bakufu apologists, some later constitutional theorists) are structurally written out of the loyalist account.
narrative_ontology:constraint_stakeholder(imperial_mandate__loyalist_restoration_reading, bakufu_delegation_reading, excluded,
    institutional, civilizational, analytical, national).
narrative_ontology:stakeholder_non_agent(imperial_mandate__loyalist_restoration_reading, bakufu_delegation_reading).

% Later scholars who examine how the loyalist reading was constructed, deployed, and then itself reinterpreted (e.g., in the 1889 Meiji Constitution's compromise between direct sovereignty and constitutional delegation) to explain both the Restoration's legitimating force and its subsequent institutional softening.
narrative_ontology:constraint_stakeholder(imperial_mandate__loyalist_restoration_reading, meiji_era_constitutional_historians, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, unifying legitimating claim around which previously fragmented anti-bakufu domains, court factions, and reform-minded samurai can coordinate military and political action against a common target, replacing centuries of divided sovereignty with one recognized locus of authority.
% TRANSFER_FUNCTION: Moves governing authority, administrative offices, tax base control, and social status from the shogunate and hereditary samurai class to the imperial court, allied restoration domains, and the newly centralized state apparatus that acts in the emperor's name.
% ABSENT_VOICES: Adherents of the bakufu delegation reading — including former shogunal officials, moderate samurai who saw dual sovereignty as workable, and foreign observers accustomed to negotiating with the shogunate as the effective sovereign — are not represented in the loyalist proclamations; their framework is treated as the error being corrected, not a live alternative to be weighed.
% DISAPPEARANCE_RATIONALE: Without this specific reading of the mandate, the Boshin War loses its legitimating premise, the abolition of the shogunate and domains loses its doctrinal justification, and the restoration coalition's claim to represent 'true' imperial will collapses — the delegation reading would remain available as the operative account of Japanese sovereignty, and centralization would have to be justified on other (e.g., purely military or pragmatic) grounds.
% FOUNDING_PROBLEM: Perceived crisis of legitimacy under the late Tokugawa bakufu: foreign gunboat pressure (Perry's arrival, unequal treaties) exposed the shogunate's inability to defend national sovereignty, and reformist factions needed a doctrinal basis to justify replacing it that was more powerful than mere factional politics.
% FOUNDING_PROBLEM_CORROBORATION: Post-Restoration constitutional drafters themselves (notably in the 1889 Meiji Constitution debates) acknowledged the need to reintroduce mediated, bureaucratic governance structures beneath the emperor, implicitly conceding the founding problem of shogunal weakness was resolved and the unmediated-sovereignty premise was thereafter treated as a legitimating fiction rather than an administrative reality; foreign diplomatic records from the 1870s-80s, written by observers outside the loyalist coalition, describe the emperor as a ratifying rather than an operating sovereign in practice.
narrative_ontology:disappearance_verdict(imperial_mandate__loyalist_restoration_reading, world_rearranges).
narrative_ontology:founding_problem_status(imperial_mandate__loyalist_restoration_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(imperial_mandate__loyalist_restoration_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(imperial_mandate__loyalist_restoration_reading, 'none', 1).
narrative_ontology:epsilon_provenance(imperial_mandate__loyalist_restoration_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(imperial_mandate__loyalist_restoration_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(imperial_mandate__loyalist_restoration_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(imperial_mandate__loyalist_restoration_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction rises sharply through the Boshin War period (1868) as the reading is weaponized to justify confiscation of domain revenues, abolition of samurai stipends, and forced reorganization of governance — then plateaus and very slightly recedes after 1873 as the state settles into routinized centralized administration rather than continuous doctrinal mobilization. Suppression peaks during the active civil war and land/status reorganization (1868-1873) when resistance to the reading was violently real (Boshin War casualties, the later Satsuma Rebellion) and remains elevated afterward because the doctrine still requires active ideological maintenance against the delegation reading's residual appeal. Theater ratio climbs steadily: as actual administrative sovereignty becomes bureaucratized under new ministries, the emperor's 'unmediated' personal rule becomes increasingly ceremonial even as the doctrine insists otherwise — this is the seed of the doctrine's own future softening in the 1889 Constitution.
 *
 * DIRECTIONALITY LOGIC:
 *   Court loyalists, restoration domains, and the imperial household are structural beneficiaries: they gain office, status, and legitimating authority precisely because the reading exists and prevails. Bakufu officials, hereditary samurai, and daimyo administrations are structural victims: the same doctrinal move that empowers the restoration coalition strips them of governing authority, caste privilege, and autonomous administration, with no path to retain standing within the new framework the reading establishes. The emperor's own seat is unusual — nominally the chief beneficiary, but identity-locked into a demanding, continuously-performed sovereignty role that constrains personal latitude more than pure ritual sovereignty would have.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (shogunal inability to answer the foreign-treaty crisis) was substantively resolved by the mid-1870s once a centralized state apparatus existed that could negotiate treaties and project state power. But the unmediated-sovereignty doctrine persisted and was even hardened in ideological instruction after the practical problem was solved, functioning increasingly as a legitimating fiction for the new Meiji bureaucratic elite rather than as a live description of how governance actually worked — the 1889 Constitution's compromise (a sovereign emperor exercising authority 'according to the provisions of this Constitution,' i.e., through ministers and a Diet) is the doctrinal system quietly reintroducing the delegation structure it had earlier delegitimized, without admitting so. This is exactly the founding_problem_status: dead / disappearance_verdict: world_rearranges divergence the R5 mismatch consumer is built to flag: the doctrine that once did real coordinating work now coexists with actual delegated bureaucratic governance it claims to forbid.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    restoration_reading_as_instrumental_construction,
    'Was the unmediated-sovereignty reading a sincere theological/constitutional claim independently arrived at, or a strategically constructed doctrine engineered by restoration-domain strategists (particularly Satsuma and Choshu ideologues) to legitimate what was substantially a military and factional power transfer?',
    'Comparative textual analysis of pre-1868 loyalist writings versus post-1868 official proclamations; tracing whether the doctrine''s content shifted opportunistically as military fortunes changed, and whether its architects privately held more instrumental views documented in personal correspondence.',
    'If substantially constructed, this reading is better classified as closer to a snare wearing coordination-doctrine cover; if sincerely and independently held prior to the military conflict, the tangled_rope classification (genuine coordination function plus real extraction) is more defensible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(restoration_reading_as_instrumental_construction, conceptual, 'Whether the loyalist reading was sincere doctrine or strategically engineered legitimation.').

omega_variable(
    sibling_reading_foreclosure_scope,
    'Does the loyalist reading''s ascendancy structurally foreclose the delegation reading permanently, or does the 1889 Meiji Constitution''s reintroduction of ministerial/Diet mediation represent the delegation reading reasserting itself under new institutional language?',
    'Constitutional-historical analysis of the 1889 Constitution''s drafting debates (Ito Hirobumi''s Prussian-influenced framework) to determine whether framers understood themselves as vindicating unmediated sovereignty in form while restoring delegated governance in substance.',
    'If the delegation reading substantively returns via constitutional mediation, the loyalist reading''s forecloses relationship to it should be read as temporary/rhetorical rather than a permanent logical foreclosure — this bears directly on how reading_relations should be interpreted diachronically.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure_scope, conceptual, 'Whether constitutional mediation after 1889 represents the delegation reading''s structural return.').

omega_variable(
    samurai_class_coalition_power,
    'Could the hereditary samurai retainers and regional daimyo administrations, as structurally similar victims of the same doctrinal move, have formed an effective resistance coalition, and to what extent did internal status stratification among samurai (upper vs. lower rank) prevent this?',
    'Analysis of Boshin War alignment patterns and the later Satsuma Rebellion (1877) to see whether samurai grievance crossed domain lines or remained fragmented by pre-existing rank and domain loyalty.',
    'If coalition was structurally prevented by internal stratification, the powerless/moderate-power victim classification for samurai retainers is reinforced; if effective coalition was possible but failed for contingent reasons, this indicates latent coalition power that the doctrine''s timing and framing successfully preempted.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(samurai_class_coalition_power, empirical, 'Whether victim groups under this reading had latent coalition power that went unrealized.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(imperial_mandate__loyalist_restoration_reading, 1853, 1889).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(impe_tr_t1853, imperial_mandate__loyalist_restoration_reading, theater_ratio, 1853, 0.2).
narrative_ontology:measurement(impe_tr_t1863, imperial_mandate__loyalist_restoration_reading, theater_ratio, 1863, 0.3).
narrative_ontology:measurement(impe_tr_t1868, imperial_mandate__loyalist_restoration_reading, theater_ratio, 1868, 0.35).
narrative_ontology:measurement(impe_tr_t1873, imperial_mandate__loyalist_restoration_reading, theater_ratio, 1873, 0.38).
narrative_ontology:measurement(impe_tr_t1881, imperial_mandate__loyalist_restoration_reading, theater_ratio, 1881, 0.42).
narrative_ontology:measurement(impe_tr_t1889, imperial_mandate__loyalist_restoration_reading, theater_ratio, 1889, 0.4).

% Extraction over time
narrative_ontology:measurement(impe_be_t1853, imperial_mandate__loyalist_restoration_reading, base_extractiveness, 1853, 0.28).
narrative_ontology:measurement(impe_be_t1863, imperial_mandate__loyalist_restoration_reading, base_extractiveness, 1863, 0.42).
narrative_ontology:measurement(impe_be_t1868, imperial_mandate__loyalist_restoration_reading, base_extractiveness, 1868, 0.61).
narrative_ontology:measurement(impe_be_t1873, imperial_mandate__loyalist_restoration_reading, base_extractiveness, 1873, 0.66).
narrative_ontology:measurement(impe_be_t1881, imperial_mandate__loyalist_restoration_reading, base_extractiveness, 1881, 0.6).
narrative_ontology:measurement(impe_be_t1889, imperial_mandate__loyalist_restoration_reading, base_extractiveness, 1889, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(impe_su_t1853, imperial_mandate__loyalist_restoration_reading, suppression_requirement, 1853, 0.25).
narrative_ontology:measurement(impe_su_t1863, imperial_mandate__loyalist_restoration_reading, suppression_requirement, 1863, 0.45).
narrative_ontology:measurement(impe_su_t1868, imperial_mandate__loyalist_restoration_reading, suppression_requirement, 1868, 0.78).
narrative_ontology:measurement(impe_su_t1873, imperial_mandate__loyalist_restoration_reading, suppression_requirement, 1873, 0.74).
narrative_ontology:measurement(impe_su_t1881, imperial_mandate__loyalist_restoration_reading, suppression_requirement, 1881, 0.66).
narrative_ontology:measurement(impe_su_t1889, imperial_mandate__loyalist_restoration_reading, suppression_requirement, 1889, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(imperial_mandate__loyalist_restoration_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(imperial_mandate__loyalist_restoration_reading, bakufu_delegation_reading).

% DUAL FORMULATION NOTE:
% This story and bakufu_delegation_reading (not authored in this file) form a two-member constraint family reading the same imperial_mandate kernel. loyalist_restoration_reading treats delegation as usurpation and requires institutional rupture (Restoration, abolition of shogunate and domains); bakufu_delegation_reading treats delegation as valid and treats the shogunate as legitimate stewardship. They must not be merged into one constraint with an averaged epsilon: their beneficiary/victim sets are near-mirror-images (restoration coalition vs. bakufu officials) and their claimed types can legitimately diverge. Link maintained via affects_constraints in both directions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
