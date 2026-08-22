% ============================================================================
% CONSTRAINT STORY: lycurgan_laws__sacral_fidelity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_lycurgan_laws__sacral_fidelity_reading, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: lycurgan_laws__sacral_fidelity_reading
 *   human_readable: Sacral-Fidelity Reading: The Lycurgan Order as Divine Immutable Ordinance
 *   domain: political_philosophy/constitutional_theory/commitment_systems
 *
 * SUMMARY:
 *   This file instantiates ONE reading of the lycurgan_laws kernel: the
 *   sacral fidelity reading, in which the Spartan settlement â the Great
 *   Rhetra received through Delphi, sealed by the founder's death-oath,
 *   guarded by the council of elders â is divine ordinance, truly
 *   unchangeable, demanding absolute adherence, with zero revision capacity
 *   treated as a virtue rather than a defect. On this reading Spartan decline
 *   is attributed to external shocks and citizen vice, never to system
 *   design. The epsilon referent is fixed: the standing arrangement under
 *   contest, the absolute-adherence regime itself, assessed by this reading's
 *   own lights â burdens are read as sacred obligation, not as rents
 *   collected by anyone, hence the moderate-low extraction value. The claim
 *   and the metrics are independent authored facts: the reading CLAIMS
 *   mountain (divine ordinance as natural law) while the metrics describe the
 *   arrangement's operation as this reading honestly assesses it, including
 *   the enforcement machinery that accumulated over the interval. Because the
 *   mountain claim carries declared beneficiaries, the story expects
 *   false-summit evaluation, and the required omega documents the
 *   natural-law-versus-constructed ambiguity. The sibling readings
 *   (demographic_trap_reading, adaptive_fiction_reading) are separate
 *   constraint files linked through network.affects_constraints; per the
 *   epsilon-invariance principle they are not folded into this one.
 *
 * KEY AGENTS:
 *   - - gerousia_elders: agenda-setting beneficiary (institutional/identity_locked) â twenty-eight life-tenured elders who interpret and guard the unalterable settlement; their rulings carry the founder's own authority
 *   - - spartiate_dual_kings: beneficiary with agenda-setting duties (institutional/identity_locked) â two hereditary houses holding command, cult, and council seats fixed by the same irrevocable settlement
 *   - - spartiate_homoioi: beneficiary-and-bearer (organized/identity_locked) â full citizens fed by allotment surplus yet bound to common messes, continuous training, and a total-life discipline; exit means social death as an inferior
 *   - - ephors: agenda-setters (institutional/constrained) â five annual overseers swearing monthly oaths against the kings, policing conduct, renewing each year the formalized hostility toward the bondsmen
 *   - - helot_underclass: primary bearer (powerless/trapped) â bonded majority population surrendering fixed produce shares under an annually renewed license for lethal coercion; no assembly voice, no standing to object
 *   - - perioikoi_free_inhabitants: secondary bearers (moderate/constrained) â free but disenfranchised communities paying tribute and serving in the army while excluded from every decision
 *   - - ancient_political_analysts: analytical observer (analytical/analytical) â Xenophon's admirative survey and Aristotle's critical chapters comparing the settlement with other poleis from outside its obligations
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(lycurgan_laws__sacral_fidelity_reading, 0.38).
domain_priors:suppression_score(lycurgan_laws__sacral_fidelity_reading, 0.55).
domain_priors:theater_ratio(lycurgan_laws__sacral_fidelity_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(lycurgan_laws__sacral_fidelity_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(lycurgan_laws__sacral_fidelity_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(lycurgan_laws__sacral_fidelity_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(lycurgan_laws__sacral_fidelity_reading, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(lycurgan_laws__sacral_fidelity_reading, resistance, 0.18).

% --- Constraint claim ---
narrative_ontology:constraint_claim(lycurgan_laws__sacral_fidelity_reading, mountain).
narrative_ontology:human_readable(lycurgan_laws__sacral_fidelity_reading, "Sacral-Fidelity Reading: The Lycurgan Order as Divine Immutable Ordinance").
narrative_ontology:topic_domain(lycurgan_laws__sacral_fidelity_reading, "political_philosophy/constitutional_theory/commitment_systems").

domain_priors:emerges_naturally(lycurgan_laws__sacral_fidelity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(lycurgan_laws__sacral_fidelity_reading, '17b238b1-b341-4894-9f20-7dfd2e015b45').
narrative_ontology:cs_kernel_codification('17b238b1-b341-4894-9f20-7dfd2e015b45', fixed_text).
narrative_ontology:cs_authority_grounding('17b238b1-b341-4894-9f20-7dfd2e015b45', lineage).
narrative_ontology:cs_interpretation_layer_present('17b238b1-b341-4894-9f20-7dfd2e015b45').
narrative_ontology:cs_reading_relation('17b238b1-b341-4894-9f20-7dfd2e015b45', lycurgan_laws__demographic_trap_reading, forecloses).
narrative_ontology:cs_reading_relation('17b238b1-b341-4894-9f20-7dfd2e015b45', lycurgan_laws__adaptive_fiction_reading, forecloses).
narrative_ontology:cs_axiom('17b238b1-b341-4894-9f20-7dfd2e015b45', foundational, rhetra_divinely_ordained_irrevocable).
narrative_ontology:cs_axiom_status(rhetra_divinely_ordained_irrevocable, holdable).
narrative_ontology:cs_axiom_grounding('17b238b1-b341-4894-9f20-7dfd2e015b45', rhetra_divinely_ordained_irrevocable, theological).
narrative_ontology:cs_axiom('17b238b1-b341-4894-9f20-7dfd2e015b45', foundational, zero_revision_capacity_is_virtue).
narrative_ontology:cs_axiom_status(zero_revision_capacity_is_virtue, holdable).
narrative_ontology:cs_axiom_grounding('17b238b1-b341-4894-9f20-7dfd2e015b45', zero_revision_capacity_is_virtue, theological).
narrative_ontology:cs_axiom('17b238b1-b341-4894-9f20-7dfd2e015b45', secondary, adherence_as_absolute_piety_duty).
narrative_ontology:cs_axiom_status(adherence_as_absolute_piety_duty, holdable).
narrative_ontology:cs_axiom_grounding('17b238b1-b341-4894-9f20-7dfd2e015b45', adherence_as_absolute_piety_duty, theological).
narrative_ontology:cs_reference_frame('17b238b1-b341-4894-9f20-7dfd2e015b45', apolline_immutable_rhetra).
narrative_ontology:cs_drift_state('17b238b1-b341-4894-9f20-7dfd2e015b45', post_leuctra_crisis, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('17b238b1-b341-4894-9f20-7dfd2e015b45', '2026-08-04T09:15:00Z').
narrative_ontology:cs_kernel_id(lycurgan_laws__sacral_fidelity_reading, lycurgan_laws).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(lycurgan_laws__sacral_fidelity_reading, gerousia_elders).
narrative_ontology:constraint_beneficiary(lycurgan_laws__sacral_fidelity_reading, spartiate_dual_kings).
narrative_ontology:constraint_beneficiary(lycurgan_laws__sacral_fidelity_reading, spartiate_homoioi).
narrative_ontology:constraint_victim(lycurgan_laws__sacral_fidelity_reading, helot_underclass).
narrative_ontology:constraint_victim(lycurgan_laws__sacral_fidelity_reading, perioikoi_free_inhabitants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(lycurgan_laws__sacral_fidelity_reading, spartiate_homoioi).
narrative_ontology:constraint_vindicates(lycurgan_laws__sacral_fidelity_reading, divine_ordination_of_the_great_rhetra).
narrative_ontology:constraint_vindicates(lycurgan_laws__sacral_fidelity_reading, delphic_apolline_sanction_of_the_settlement).
narrative_ontology:constraint_vindicates(lycurgan_laws__sacral_fidelity_reading, founder_death_oath_irrevocability).
narrative_ontology:constraint_vindicates(lycurgan_laws__sacral_fidelity_reading, mixed_constitution_stability_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Twenty-eight men over sixty, elected for life, who deliberate on every proposal, judge capital cases, and guard the settlement's wording. Because nothing may be altered, their interpretations carry the founder's own authority, and their standing rests on the order they interpret. Office lasts until death; stepping aside means returning to ordinary citizen life at an age past soldiering, and their whole adult identity was formed by the education and councils that lead to this seat.
narrative_ontology:constraint_stakeholder(lycurgan_laws__sacral_fidelity_reading, gerousia_elders, agenda_setter,
    institutional, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(lycurgan_laws__sacral_fidelity_reading, gerousia_elders, beneficiary).

% Two hereditary royal houses holding army command, chief priesthoods, and ex-officio seats in the elder council. Their prerogatives are fixed by the same unalterable settlement that fixes everything else, and each reign is hedged by monthly oaths exchanged with the annual overseers. Renouncing the station would mean dissolving a sacral office fused with the house's identity across centuries.
narrative_ontology:constraint_stakeholder(lycurgan_laws__sacral_fidelity_reading, spartiate_dual_kings, beneficiary,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(lycurgan_laws__sacral_fidelity_reading, spartiate_dual_kings, agenda_setter).

% Full male citizens supported by hereditary allotments worked by bondsmen. They eat at common messes to which they contribute fixed shares from their own lots, train continuously from boyhood, are barred from productive trades and conspicuous wealth, and vote in the assembly â though they may only approve or reject what others place before them. Leaving means becoming an inferior: loss of mess, rank, and marriage prospects. The state education fuses their sense of self with the way of life from age seven.
narrative_ontology:constraint_stakeholder(lycurgan_laws__sacral_fidelity_reading, spartiate_homoioi, beneficiary,
    organized, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(lycurgan_laws__sacral_fidelity_reading, spartiate_homoioi, payer).

% Five annual overseers drawn from the citizen body. They swear a monthly oath against each king, may convene and indict, watch the conduct of all ranks, and each year renew a formal declaration of hostile intent toward the bondsmen that licenses lethal action without ritual penalty. Their term is one year, after which they revert to private standing; while in office they police adherence to the established ways rather than originate change.
narrative_ontology:constraint_stakeholder(lycurgan_laws__sacral_fidelity_reading, ephors, agenda_setter,
    institutional, immediate, constrained, national).

% Bonded agriculturalists attached to citizen allotments, concentrated in conquered Messenia. They surrender a fixed share of produce, serve as attendants and light troops in war, endure ritual humiliations, and live under the annually renewed license for killing. A few earn release through distinguished military service as neodamodeis; the rest are bound to the soil they till. They have no assembly, no advocate, and no standing anywhere in the constitutional conversation; their periodic revolts are the only voice available to them.
narrative_ontology:constraint_stakeholder(lycurgan_laws__sacral_fidelity_reading, helot_underclass, payer,
    powerless, generational, trapped, regional).
narrative_ontology:stakeholder_secondary_role(lycurgan_laws__sacral_fidelity_reading, helot_underclass, excluded).

% Free communities scattered through Laconia and Messenia. They farm, manufacture, and trade â occupations barred to citizens â pay tribute, and serve in the army, but hold no vote in the citizen assembly and no share in the decisions that govern them. Their communities manage their own internal affairs. Leaving would mean abandoning their land and trading networks for an uncertain reception abroad.
narrative_ontology:constraint_stakeholder(lycurgan_laws__sacral_fidelity_reading, perioikoi_free_inhabitants, payer,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(lycurgan_laws__sacral_fidelity_reading, perioikoi_free_inhabitants, excluded).

% Analysts of Greek politics writing from outside the settlement's obligations â Xenophon's admirative survey of the Spartan way, Aristotle's critical chapters on its land tenure, officeholders, and subject classes. They compare the whole architecture (allotments, education, councils, bondsmen, subject communities) with other poleis and can name features the participants cannot see from inside their stations.
narrative_ontology:constraint_stakeholder(lycurgan_laws__sacral_fidelity_reading, ancient_political_analysts, observer,
    analytical, biographical, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(lycurgan_laws__sacral_fidelity_reading, spartiate_homoioi).
narrative_ontology:fixing_cost_class(lycurgan_laws__sacral_fidelity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The settlement solves a real collective-action problem: how a small citizen body maintains permanent military readiness, internal cohesion, and control over a bonded population many times its number. Uniform state education, common messes, standardized equipment, and frozen land lots suppress the economic differentiation that had produced chronic faction, and the irrevocable oath precommits every future generation against loosening the discipline each would privately prefer to relax.
% TRANSFER_FUNCTION: Moves surplus produce from bonded laborers on hereditary allotments to the citizen households that hold them, funding leisured full-time citizenship; moves tribute and military service from the free resident communities to the center; moves deference and obedience from citizens and subjects alike to the elder council and royal houses; allocates honor and standing according to adherence.
% ABSENT_VOICES: The bonded majority has no assembly, no advocate, and no standing to object â they are present only as the object of the annual war declaration; the free resident communities are governed without a vote; within the citizen body the young and the assembly itself can only approve or reject, never initiate; and the founder's oath forecloses even the voices of generations not yet born. The dissenters who exist are outside the walls (revolt in Messenia) or outside the citizen roll (the shirkers who chose exile).
% DISAPPEARANCE_RATIONALE: If the settlement and its sanctity vanished overnight, the allotment economy would dissolve as bonded shares ceased flowing, the common messes and state education would lose their material base, the elder council and royal houses would lose the authority the irrevocable frame confers, and the citizen army â the polis's entire defensive and hegemonic capacity â would cease to exist as an institution; Messenia would be lost and the polis as constituted would rearrange into something unrecognizable within a generation.
% FOUNDING_PROBLEM: Chronic internal civil strife between the dual kings, the aristocratic council, and the demos in a small polis that also needed to defend itself and, after the conquest of Messenia, to control a subject population many times its number; the settlement was built to end faction by equalizing land into hereditary lots, mixing the constitution across king, council, and assembly, and binding all parties under an oath that no generation could undo.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: Thucydides attests the internal stability the settlement achieved, and Aristotle's Politics attests that its later operation centered on the bonded class and military primacy rather than the original concord â both writing from seats outside the order's offices. Adherents of the sacral frame attest the founding vigilance remains eternally live; critics attest the founding strife was solved within generations and the arrangement persisted for other purposes. No voice from the bonded majority survives anywhere in the record; the total absence of witnesses from the largest subject population is itself signal about whom the arrangement came to serve.
narrative_ontology:disappearance_verdict(lycurgan_laws__sacral_fidelity_reading, world_rearranges).
narrative_ontology:founding_problem_status(lycurgan_laws__sacral_fidelity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(lycurgan_laws__sacral_fidelity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(lycurgan_laws__sacral_fidelity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(lycurgan_laws__sacral_fidelity_reading, 0.38, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(lycurgan_laws__sacral_fidelity_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(lycurgan_laws__sacral_fidelity_reading, ExtMetricName, E),
    domain_priors:suppression_score(lycurgan_laws__sacral_fidelity_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(lycurgan_laws__sacral_fidelity_reading),
    narrative_ontology:constraint_metric(lycurgan_laws__sacral_fidelity_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(lycurgan_laws__sacral_fidelity_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(lycurgan_laws__sacral_fidelity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored at 0.38 (end-state, matching the final series point) because this reading assesses the arrangement's burdens as sacred duties owed rather than rents collected: the citizen's discipline, the mess dues, the fixed helot shares are, in this frame, the divinely allotted price of order, with conceded abuse attributed to individual vice rather than design. Suppression is authored at 0.55 as a raw, unscaled structural property: the reading holds that sanctity and habituation carry the load while custodial machinery (overseers, the annual war declaration, prosecution of shirkers) accumulated around it â the rising suppression_requirement series traces that machinery's growth, which the reading interprets as a symptom of declining piety, not as the constraint's load-bearing wall; hence requires_active_enforcement is authored false. Theater is 0.30: observance stayed sincere for most of the interval and turned formal only late, as forms persisted while practice decayed. Accessibility_collapse is 0.85 because within the sacral frame alternatives do not merely cost more â proposing revision of divine ordinance is category-error and impiety, so alternatives collapse almost completely once the frame is accepted. Resistance is 0.18: the Messenian bondsmen waged coalition revolts lasting generations and shirkers self-exiled, but no internal movement to revise the laws arose for centuries. All three series run on one shared seven-point grid (0, 70, 140, 210, 280, 350, 430) so every metric is authored at every examined time point; the interval anchors 0 at the traditional codification under the founder's settlement and 430 at the aftermath of Leuctra.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute very differently. From the council elders' and kings' positions the settlement is the sacred order they embody and interpret â subsidy and standing, near-zero experienced extraction. From the bondsmen's position the same settlement is total extraction with lethal enforcement and no exit. The full citizens straddle: fed by allotment surplus yet consumed by the discipline that feeds them standing. The reading itself speaks from the devout citizen seat and experiences piety, not extraction â which is precisely why its unitary mountain claim must be tested against the per-seat computations the engine derives from the structural data; the divergence between the claimed type and the computed per-seat types is the measurement this file exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (council elders, royal houses, full citizens) derive low directionality for those seats; victim declarations (bondsmen, free residents) derive high directionality. Two overrides correct derivations the role arrays cannot see. First, the full citizens are declared beneficiaries, which would push them near the beneficiary pole, but they also bear the total-life costs â childhood segregation, mess contributions from their own lots, prohibition of productive work, lifelong service â so their net position is only mildly subsidized (organized overridden to 0.42). Second, the free residents are declared victims, which would push them near the target pole, but they retain communal self-government and the economic niches citizens are barred from; the extraction's center of gravity is the bonded class, not them (moderate overridden to 0.62). The bondsmen need no override: powerless, trapped, declared victims â the derivation already places them at the extreme target end. The annual overseers are agenda-setters outside the beneficiary array; no override is applied because any institutional-atom override would also strike the elders and kings, whose derived positions are correct.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading denies mandatrophy outright: the mandate is eternal, so the question of a function outliving its purpose cannot arise within the frame. The R5 interview is nonetheless answered honestly at story level â the founding problem (chronic internal faction between kings, council, and demos) was resolved within a few generations, and the arrangement persisted roughly four centuries thereafter serving bondsmen-control and military primacy; the status is therefore authored contested, not dead, because the parties genuinely dispute whether the vigilance the frame demands is ever obsolete. The mismatch consumer reads status x disappearance_verdict: contested paired with world_rearranges raises no zombie flag, correctly â the arrangement's persistence is not inertial performance but actively maintained order. The classification apparatus prevents mislabeling in both directions: if the sanctity is real and self-sustaining, the mountain claim certifies; if the rigidity instead concentrates standing in identifiable officeholders who would lose it under revision, the declared beneficiaries route the story through false-summit evaluation and the type recomputes as hybrid coordination/extraction. The corpus measures which, and this file supplies the structural data without prejudging it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    naturality_vs_entrenchment,
    'Is the order''s immutability a self-enforcing, natural-law-like regularity that would persist regardless of defenders, or a constructed constitutional arrangement whose rigidity concentrates standing, office, and honor in identifiable officeholders?',
    'Comparative constitutional analysis: whether analogous arrangements persist without sacral framing, plus enforcement-dependence testing (does adherence survive when the custodial organs weaken?). Beneficiary presence on a mountain claim routes this through false-summit evaluation.',
    'If the rigidity is constructed and the officeholders are its concentrated gainers, the mountain claim fails summit certification and the arrangement recomputes as a hybrid coordination/extraction structure; if genuinely self-enforcing, the mountain claim stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(naturality_vs_entrenchment, empirical, 'Natural-law versus constructed-constraint ambiguity of the immutability doctrine.').

omega_variable(
    kernel_reading_commitment,
    'This constraint is one reading of the lycurgan_laws kernel (sacral_fidelity_reading). Would the sibling readings re-author the same standing arrangement with different epsilon and type — and where exactly is the disagreement located?',
    'Read the sibling files directly: demographic_trap_reading attributes the collapse to the design''s unrevisability and would author high extraction over the same referent; adaptive_fiction_reading holds the immutability doctrine is a noble lie over covert adaptation and would author a high theater ratio. The disagreement is located in (a) the causal attribution of Spartan decline and (b) the reality-status of the immutability itself.',
    'No resolution inside this file by construction — the readings are separate constraints linked by network edges; cross-reading comparison is the corpus-level measurement.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: which kernel, which reading, what siblings would change.').

omega_variable(
    decline_attribution_dispute,
    'Does the 5thâ4th century contraction of the citizen body (from roughly eight thousand full citizens to about a thousand after Leuctra) trace to the settlement''s own design features â frozen land lots passing to daughters, no provision for replenishment, admission barriers â or to external shocks and citizen vice (earthquake, Messenian wars, hegemonic overextension, disciplinary decay)?',
    'Demographic and land-tenure reconstruction from estate records, succession patterns, and casualty figures; comparison with poleis facing similar shocks that retained revision capacity.',
    'If design-attributed, this reading''s low-extraction assessment is untenable and the sibling demographic reading gains decisive support; if shock-and-vice-attributed, the reading''s framing survives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(decline_attribution_dispute, empirical, 'Whether decline was systemic (design) or adventitious (shocks, vice).').

omega_variable(
    founder_oath_historicity,
    'Is the death-pact oath â the founder extracting from kings, council, and assembly a promise to change nothing until his return from Delphi, then never returning â a historical mechanism or a retrojected legend accounting for why no revision ever occurred?',
    'Source criticism of the transmission chain (Herodotean and Plutarchan traditions against earlier fragments such as Tyrtaios''s quotation of the rhetra); dating of when the no-change norm first appears as a stated rule.',
    'If legendary, the irrevocability mechanism was accreted rather than constitutive, weakening the claim that zero revision capacity was a founding design virtue; if historical, the precommitment reading of the founding strengthens.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(founder_oath_historicity, empirical, 'Historicity of the oath said to seal the laws against alteration.').

omega_variable(
    internalized_vs_structural_compliance,
    'Was adherence carried by internalized formation (state education from childhood, common messes, fused civic identity) or by external enforcement (overseers'' surveillance, formalized hostility toward the bondsmen, prosecution of shirkers)?',
    'Post-collapse trajectory: after Leuctra, when enforcement capacity and the economic base weakened together, adherence dissolved within a generation rather than persisting â indicating a large structural component beneath the internalized surface.',
    'If compliance was substantially internalized, the measured suppression understates the constraint''s grip and exit was psychologically costlier than institutions alone suggest; if structural, dismantling the enforcement machinery suffices to dissolve it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_vs_structural_compliance, empirical, 'Split between internalized paideia and external enforcement in sustaining adherence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(lycurgan_laws__sacral_fidelity_reading, 0, 430).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lycurgan_sacral_fid_tr_t0, lycurgan_laws__sacral_fidelity_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(lycurgan_sacral_fid_tr_t0, observed).
narrative_ontology:measurement(lycurgan_sacral_fid_tr_t70, lycurgan_laws__sacral_fidelity_reading, theater_ratio, 70, 0.1).
narrative_ontology:measurement_basis(lycurgan_sacral_fid_tr_t70, observed).
narrative_ontology:measurement(lycurgan_sacral_fid_tr_t140, lycurgan_laws__sacral_fidelity_reading, theater_ratio, 140, 0.13).
narrative_ontology:measurement_basis(lycurgan_sacral_fid_tr_t140, observed).
narrative_ontology:measurement(lycurgan_sacral_fid_tr_t210, lycurgan_laws__sacral_fidelity_reading, theater_ratio, 210, 0.17).
narrative_ontology:measurement_basis(lycurgan_sacral_fid_tr_t210, observed).
narrative_ontology:measurement(lycurgan_sacral_fid_tr_t280, lycurgan_laws__sacral_fidelity_reading, theater_ratio, 280, 0.21).
narrative_ontology:measurement_basis(lycurgan_sacral_fid_tr_t280, observed).
narrative_ontology:measurement(lycurgan_sacral_fid_tr_t350, lycurgan_laws__sacral_fidelity_reading, theater_ratio, 350, 0.26).
narrative_ontology:measurement_basis(lycurgan_sacral_fid_tr_t350, observed).
narrative_ontology:measurement(lycurgan_sacral_fid_tr_t430, lycurgan_laws__sacral_fidelity_reading, theater_ratio, 430, 0.3).
narrative_ontology:measurement_basis(lycurgan_sacral_fid_tr_t430, observed).

% Extraction over time
narrative_ontology:measurement(lycurgan_sacral_fid_be_t0, lycurgan_laws__sacral_fidelity_reading, base_extractiveness, 0, 0.24).
narrative_ontology:measurement_basis(lycurgan_sacral_fid_be_t0, observed).
narrative_ontology:measurement(lycurgan_sacral_fid_be_t70, lycurgan_laws__sacral_fidelity_reading, base_extractiveness, 70, 0.26).
narrative_ontology:measurement_basis(lycurgan_sacral_fid_be_t70, observed).
narrative_ontology:measurement(lycurgan_sacral_fid_be_t140, lycurgan_laws__sacral_fidelity_reading, base_extractiveness, 140, 0.29).
narrative_ontology:measurement_basis(lycurgan_sacral_fid_be_t140, observed).
narrative_ontology:measurement(lycurgan_sacral_fid_be_t210, lycurgan_laws__sacral_fidelity_reading, base_extractiveness, 210, 0.32).
narrative_ontology:measurement_basis(lycurgan_sacral_fid_be_t210, observed).
narrative_ontology:measurement(lycurgan_sacral_fid_be_t280, lycurgan_laws__sacral_fidelity_reading, base_extractiveness, 280, 0.34).
narrative_ontology:measurement_basis(lycurgan_sacral_fid_be_t280, observed).
narrative_ontology:measurement(lycurgan_sacral_fid_be_t350, lycurgan_laws__sacral_fidelity_reading, base_extractiveness, 350, 0.36).
narrative_ontology:measurement_basis(lycurgan_sacral_fid_be_t350, observed).
narrative_ontology:measurement(lycurgan_sacral_fid_be_t430, lycurgan_laws__sacral_fidelity_reading, base_extractiveness, 430, 0.38).
narrative_ontology:measurement_basis(lycurgan_sacral_fid_be_t430, observed).

% Suppression requirement over time
narrative_ontology:measurement(lycurgan_sacral_fid_su_t0, lycurgan_laws__sacral_fidelity_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement_basis(lycurgan_sacral_fid_su_t0, observed).
narrative_ontology:measurement(lycurgan_sacral_fid_su_t70, lycurgan_laws__sacral_fidelity_reading, suppression_requirement, 70, 0.2).
narrative_ontology:measurement_basis(lycurgan_sacral_fid_su_t70, observed).
narrative_ontology:measurement(lycurgan_sacral_fid_su_t140, lycurgan_laws__sacral_fidelity_reading, suppression_requirement, 140, 0.28).
narrative_ontology:measurement_basis(lycurgan_sacral_fid_su_t140, observed).
narrative_ontology:measurement(lycurgan_sacral_fid_su_t210, lycurgan_laws__sacral_fidelity_reading, suppression_requirement, 210, 0.35).
narrative_ontology:measurement_basis(lycurgan_sacral_fid_su_t210, observed).
narrative_ontology:measurement(lycurgan_sacral_fid_su_t280, lycurgan_laws__sacral_fidelity_reading, suppression_requirement, 280, 0.42).
narrative_ontology:measurement_basis(lycurgan_sacral_fid_su_t280, observed).
narrative_ontology:measurement(lycurgan_sacral_fid_su_t350, lycurgan_laws__sacral_fidelity_reading, suppression_requirement, 350, 0.49).
narrative_ontology:measurement_basis(lycurgan_sacral_fid_su_t350, observed).
narrative_ontology:measurement(lycurgan_sacral_fid_su_t430, lycurgan_laws__sacral_fidelity_reading, suppression_requirement, 430, 0.55).
narrative_ontology:measurement_basis(lycurgan_sacral_fid_su_t430, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(lycurgan_laws__sacral_fidelity_reading, identity_coordination).
narrative_ontology:affects_constraint(lycurgan_laws__sacral_fidelity_reading, demographic_trap_reading).
narrative_ontology:affects_constraint(lycurgan_laws__sacral_fidelity_reading, adaptive_fiction_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'the Lycurgan laws' conflates three structurally distinct claims about one kernel. This file (sacral_fidelity_reading) authors the arrangement as divine immutable ordinance â reading-indexed epsilon 0.38 over the fixed referent of the standing absolute-adherence regime. demographic_trap_reading authors the same referent as a brittle unrevisable system whose design caused collapse (high epsilon, design-defect attribution). adaptive_fiction_reading authors the immutability doctrine itself as theatrical cover for covert adaptation (high theater_ratio). The upstream member by empirical confidence is the sacral reading's own textual tradition (the rhetra fragment quoted by Tyrtaios, the Plutarchan biography), which both siblings cite as the object of their critique; all three files link one another through network.affects_constraints so contamination and cross-reading comparison propagate through the family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(lycurgan_laws__sacral_fidelity_reading, organized, 0.42).
constraint_indexing:directionality_override(lycurgan_laws__sacral_fidelity_reading, moderate, 0.62).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
