% ============================================================================
% CONSTRAINT STORY: dueling_disappearance_mechanism__overdetermined_composite_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dueling_disappearance_mechanism__overdetermined_composite_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: dueling_disappearance_mechanism__overdetermined_composite_reading
 *   human_readable: Dueling's Decline via Overdetermined Causal Mechanisms (1790-1860)
 *   domain: historical_sociology/cultural_anthropology/legal_history
 *
 * SUMMARY:
 *   This constraint story narrates dueling's decline (1790–1860) as causally
 *   overdetermined: multiple independent sufficient conditions—legal
 *   prohibition, institutional modernization (courts, court-martial),
 *   cultural reframing (honor as barbarism), and Civil War trauma (military
 *   virtue irrelevant at industrial scale)—acted simultaneously and
 *   reinforced each other. No single mechanism was individually necessary;
 *   the constraint's persistence rested on the convergence of all four. This
 *   reading differs from the contraction reading (which attributes decline to
 *   cultural shift alone, honor becoming unthinkable) and the
 *   institutional_displacement reading (which attributes decline to courts
 *   and libel law outcompeting dueling as remedy). This reading claims the
 *   causal pathways were non-separable and measuring an ε against a single
 *   mechanism misses the tangled structure: legal prohibition activated
 *   beneficiaries (reformers, state); institutional substitution activated
 *   beneficiaries (courts, bourgeoisie); cultural shift activated
 *   beneficiaries (moral advocates); war destroyed the social base (gentry,
 *   officers). The victims—honor-culture practitioners, gentry, officer
 *   corps—faced extraction from all four simultaneously, each of which
 *   claimed a different justification (rule of law, efficiency, morality,
 *   necessity), making resistance fragmented and unable to halt decline
 *   through any single channel.
 *
 * KEY AGENTS:
 *   - honor_culture_practitioners: Gentry and officer corps whose status and authority depended on honor-vindication through dueling; faced legal prosecution, institutional substitution (courts and court-martial), cultural delegitimization, and loss of social relevance after the Civil War.
 *   - institutional_legal_reformers: Legislatures, bar associations, and courts that benefited from criminalizing dueling by consolidating dispute-resolution authority; authored legal prohibitions and prosecuted violations.
 *   - industrial_bourgeoisie: Merchants and manufacturers who benefited from shift from honor-based to contract-based dispute resolution; financed and promoted anti-dueling legislation.
 *   - nation_state_consolidators: Central governments (France, U.S., Prussia) that used anti-dueling law as a mechanism to monopolize violence and disarm regional gentry power; prosecuted, exiled, or executed duelists.
 *   - anti_dueling_cultural_advocates: Religious leaders, philosophers, journalists, and reformers who reframed honor-seeking through violence as uncivilized, unchristian, or irrational; shaped moral narrative.
 *   - Civil War: The American Civil War (1861–1865) destroyed the gentry power base, demonstrated that military victory depended on industrial scale not officer courage, and delegitimized the martial virtue dueling embodied.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dueling_disappearance_mechanism__overdetermined_composite_reading, 0.58).
domain_priors:suppression_score(dueling_disappearance_mechanism__overdetermined_composite_reading, 0.71).
domain_priors:theater_ratio(dueling_disappearance_mechanism__overdetermined_composite_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__overdetermined_composite_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__overdetermined_composite_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__overdetermined_composite_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__overdetermined_composite_reading, accessibility_collapse, 0.67).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__overdetermined_composite_reading, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dueling_disappearance_mechanism__overdetermined_composite_reading, tangled_rope).
narrative_ontology:human_readable(dueling_disappearance_mechanism__overdetermined_composite_reading, "Dueling's Decline via Overdetermined Causal Mechanisms (1790-1860)").
narrative_ontology:topic_domain(dueling_disappearance_mechanism__overdetermined_composite_reading, "historical_sociology/cultural_anthropology/legal_history").

domain_priors:requires_active_enforcement(dueling_disappearance_mechanism__overdetermined_composite_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dueling_disappearance_mechanism__overdetermined_composite_reading, '24754b0b-5bf4-4936-88ee-de158735470f').
narrative_ontology:cs_kernel_codification('24754b0b-5bf4-4936-88ee-de158735470f', distributed).
narrative_ontology:cs_authority_grounding('24754b0b-5bf4-4936-88ee-de158735470f', distributed).
narrative_ontology:cs_reading_relation('24754b0b-5bf4-4936-88ee-de158735470f', dueling_disappearance_mechanism__contraction_reading, influences).
narrative_ontology:cs_reading_relation('24754b0b-5bf4-4936-88ee-de158735470f', dueling_disappearance_mechanism__institutional_displacement_reading, influences).
narrative_ontology:cs_axiom('24754b0b-5bf4-4936-88ee-de158735470f', foundational, causal_non_separability).
narrative_ontology:cs_axiom_status(causal_non_separability, holdable).
narrative_ontology:cs_axiom_grounding('24754b0b-5bf4-4936-88ee-de158735470f', causal_non_separability, empirically_contingent).
narrative_ontology:cs_axiom('24754b0b-5bf4-4936-88ee-de158735470f', secondary, simultaneous_mechanism_amplification).
narrative_ontology:cs_axiom_status(simultaneous_mechanism_amplification, holdable).
narrative_ontology:cs_axiom_grounding('24754b0b-5bf4-4936-88ee-de158735470f', simultaneous_mechanism_amplification, empirically_contingent).
narrative_ontology:cs_reference_frame('24754b0b-5bf4-4936-88ee-de158735470f', honor_culture_dispute_settlement).
narrative_ontology:cs_drift_state('24754b0b-5bf4-4936-88ee-de158735470f', post_civil_war_industrial_order, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('24754b0b-5bf4-4936-88ee-de158735470f', '').
narrative_ontology:cs_kernel_id(dueling_disappearance_mechanism__overdetermined_composite_reading, dueling_disappearance_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__overdetermined_composite_reading, institutional_legal_reformers).
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__overdetermined_composite_reading, industrial_bourgeoisie).
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__overdetermined_composite_reading, nation_state_consolidators).
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__overdetermined_composite_reading, anti_dueling_cultural_advocates).
narrative_ontology:constraint_victim(dueling_disappearance_mechanism__overdetermined_composite_reading, honor_culture_practitioners).
narrative_ontology:constraint_victim(dueling_disappearance_mechanism__overdetermined_composite_reading, gentry_elites).
narrative_ontology:constraint_victim(dueling_disappearance_mechanism__overdetermined_composite_reading, military_officer_corps).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Elites (gentry, military officers) for whom honor vindication through dueling was intrinsic to identity and status maintenance. Exit meant accepting public insult, loss of reputation, and loss of social standing. The constraint's mechanisms targeted their practices directly: legal prohibition forbade the action; institutional substitutes (libel law, courts of honor) offered alternatives that did not restore the symbolic capital dueling had provided; cultural shift reframed honor-seeking as barbarism; Civil War trauma delegitimized the martial virtue dueling embodied.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__overdetermined_composite_reading, honor_culture_practitioners, payer,
    powerful, biographical, identity_locked, regional).

% Legislatures, bar associations, and legal system architects who benefited from dueling's criminalization by consolidating dispute resolution into courts and legal procedure. Their authority and fee structures expanded as dueling declined and litigious remedy became the socially sanctioned channel for vindicating rights. They authored the legal prohibitions and enforced them through prosecution and social stigma.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__overdetermined_composite_reading, institutional_legal_reformers, beneficiary,
    institutional, generational, arbitrage, national).

% Merchants and manufacturers who benefited from the shift from honor-based (honor, face, reputation) to contract-based (property rights, predictable enforcement, written law) dispute resolution. The death of dueling meant disputes over contracts, debt, and commercial honor could be settled by courts and written agreements rather than violence, lowering transaction costs and enabling larger-scale business. They financed and promoted anti-dueling legislation.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__overdetermined_composite_reading, industrial_bourgeoisie, beneficiary,
    organized, generational, arbitrage, national).

% Central governments (French and U.S. federal authorities, Prussian state) that used anti-dueling law as one mechanism to monopolize violence and disarm regional gentry power. Dueling was illegal in most jurisdictions by the mid-1800s, but prosecution intensity varied by state capacity and threat perception. The Civil War (U.S.) destroyed the gentry power base and removed enforcement ambiguity. State actors prosecuted, exiled, or executed duelists to establish the principle that private revenge and honor claims were subordinate to state justice.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__overdetermined_composite_reading, nation_state_consolidators, agenda_setter,
    institutional, civilizational, analytical, national).

% Religious leaders, philosophers, journalists, and reformers who reframed honor-seeking through violence as uncivilized, unchristian, or irrational. They did not have state power but shaped moral narrative: dueling became coded as barbarism, cowardice (if refusing), or insanity (if accepting) rather than as virtue. They authored the cultural shift that made dueling unthinkable even when legal prohibition was lax or unenforced.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__overdetermined_composite_reading, anti_dueling_cultural_advocates, beneficiary,
    moderate, biographical, mobile, regional).

% Large landholders and regional aristocrats whose status and dispute-resolution authority had relied on dueling as a practiced mechanism of vindicating claims and maintaining face. As legal prohibition, institutional substitutes, and cultural reframing converged, their status claims were increasingly undefendable through dueling. Some chose death (or exile) rather than accept alternative remedy; others adapted to litigation and property law but lost the authority dueling had granted them.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__overdetermined_composite_reading, gentry_elites, payer,
    powerful, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(dueling_disappearance_mechanism__overdetermined_composite_reading, gentry_elites, observer).

% Officers whose professional identity was bound to honor, courage, and the willingness to risk death for vindication. Dueling was how rank insults were settled and martial virtue demonstrated. Anti-dueling law, institutional court-martial procedure, and cultural shift all targeted military honor claims. The U.S. Civil War accelerated the shift by showing that military glory had industrial rather than martial sources. Officers faced prosecution or execution for dueling (e.g., the execution of Major Frederick Winthrop in 1859 made the enforcement real).
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__overdetermined_composite_reading, military_officer_corps, payer,
    organized, biographical, identity_locked, national).

% Historians, legal scholars, and contemporary observers who track the decline and decompose its causes. They hold no enforcement power but hold the interpretive frame for understanding whether dueling fell to law, institutions, culture, or war separately or in combination. The overdetermined reading is their working hypothesis; it is contested by advocates for each single-mechanism reading.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__overdetermined_composite_reading, civil_society_observers, observer,
    analytical, biographical, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Multiple independent mechanisms each offered a solution to a different aspect of dispute resolution: law offered legal remedy and state-monopolized justice; institutions offered efficient, written procedures and property-based remedy; culture offered moral narrative that delegitimized violence as problem-solving; war forced reorganization of the social hierarchy that had depended on honor-vindication. Individually, each was a coordination solution to a real problem. Collectively, they were a tangled coordination: no single mechanism fully replaced dueling's honor-restoration function, but together they closed all paths to honor-based status vindication and opened alternative paths (legal standing, property, credentials, military rank). The coordination problem dueling had solved (how does someone answer insult and restore status?) was no longer addressable through its original mechanism because all four new mechanisms made the old mechanism unthinkable, unenforceable, illegal, and socially irrelevant simultaneously.
% TRANSFER_FUNCTION: The arrangement transferred authority to answer insults and vindicate status from the dueling community (gentry, officers) to four new seats: (1) courts and legal system (law); (2) institutional procedures and professions (institutions); (3) clergy, philosophers, and press (culture); (4) military-industrial state (war). It also transferred the form of status-vindication from corporal (death risk as proof of honor) to bureaucratic/pecuniary (rank, credentials, written judgment, property compensation). In this sense, the arrangement moved status capital from the honor-culture practitioners to the industrial bourgeoisie and professional classes.
% ABSENT_VOICES: Those who benefited from honor-culture dispute-settlement outside the gentry and officer class (rural servants, farmers, women, enslaved people in the American South) were nominally absent from the reform conversation. While they could not duel (gender, class, legal status barred them), insults to them had no sanctioned answering mechanism in either honor or law regimes. Women faced particular absence: barred from dueling before and after, and barred from legal remedy of insult through most of the period. The reformers did not address what alternatives existed for non-dueling populations. Rural populations in post-Civil-War South faced absence: traditional honor mechanisms were destroyed but legal remedy in federal courts was slow and hostile.
% DISAPPEARANCE_RATIONALE: If the four mechanisms had not converged and reinforced each other, dueling would have persisted much longer: strong legal prohibition without cultural shift would have driven practice underground but not eliminated it (Germany's experience); strong cultural shift without legal enforcement would have left dueling as an elite residual (France's experience pre-1830); strong institutional substitution without political consolidation would have left dueling as alternative remedy for insults courts could not address; war alone would not have eliminated honor-culture if the gentry class had survived intact. The convergence of all four created mutual amplification: law gave courts legitimacy, institutions gave culture a claim to progress, culture made prosecution acceptable, and war destroyed the social base. Remove any one mechanism and dueling's lifecycle extends substantially. The world without the constraint is one where honor-culture dispute-settlement persists as a viable (if legally fraught) option; with the constraint, it disappeared entirely from legitimate practice.
% FOUNDING_PROBLEM: In honor-based social orders, insult that could not be answered created cascading loss of standing and authority. Dueling was the institutionalized answer: a public ritual that allowed the insulted to risk death and thereby restore face. As long as honor was the primary currency of status (in gentry republics, military hierarchies, and small-scale face-to-face communities), dueling served a real coordination function: it settled disputes that no other mechanism could satisfy, and it did so in a culturally intelligible way (the one with the courage to risk death wins the status claim).
% FOUNDING_PROBLEM_CORROBORATION: Legal historians (Aya, Greenberg, Wyatt-Brown) document that by 1880, no major jurisdiction treated dueling as a legitimate remedy for insult; military codes explicitly forbade it; legal remedy through courts was universal; and cultural narrative treated honor-seeking through violence as barbarism. Sociologists (Elias, Foucault) argue that the legitimacy of corporal violence as status-proof eroded across multiple institutional domains simultaneously. Military historians note that by the Civil War era, officer courage had become orthogonal to military success (industrial scale, logistics, artillery dominance made individual bravery irrelevant), which delegitimized the martial virtue dueling had embodied. Witnesses from outside the honor-culture (journalists, clergy, legal reformers) attest that the founding problem—how to answer insult in a way that restores honor—is no longer live: insults are answered through litigation or ignored entirely. The founding problem is attested as dead by every seat except honor-culture nostalgia.
narrative_ontology:disappearance_verdict(dueling_disappearance_mechanism__overdetermined_composite_reading, world_rearranges).
narrative_ontology:founding_problem_status(dueling_disappearance_mechanism__overdetermined_composite_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dueling_disappearance_mechanism__overdetermined_composite_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(dueling_disappearance_mechanism__overdetermined_composite_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dueling_disappearance_mechanism__overdetermined_composite_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(dueling_disappearance_mechanism__overdetermined_composite_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(dueling_disappearance_mechanism__overdetermined_composite_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness starts low (0.35) in the late 1700s when dueling prohibition was unenforced and cultural stigma was weak, rising to 0.72 by 1860 as legal enforcement intensified, institutional substitutes consolidated, cultural narrative became hegemonic, and the Civil War eliminated the gentry class's structural power. Suppression tracks extraction: legal penalties began modest and rose steeply; institutional court-martial provided alternatives; cultural shame made prosecution socially costless; war trauma made honor claims explicitly unthinkable. Theater ratio rises from 0.28 to 0.51: in early phases, anti-dueling enforcement was performative (symbolic prosecution with light penalties); by 1860, it was structural (execution of Major Winthrop, permanent professional prohibition in military). The coercion grid shows suppression rising across all four levels: individual (officers face prosecution), organizational (military codes tighten), class (gentry lose standing), structural (state monopoly on violence is consolidated). Resistance falls from 0.72 to 0.28 as alternatives offer safer answers to insult and the cultural narrative makes resistance unspeakable. This trajectory models a constraint sustained by non-separable causal mechanisms: removing any single mechanism (law, institutions, culture, or war) would have extended dueling's lifecycle substantially, but their convergence made decline rapid and total.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the beneficiary seats diverge dramatically. From the state/reformer seat, dueling's decline is a straightforward victory: law won, civilization prevailed, violence was disarmed. From the officer/gentry seat, dueling's decline is a catastrophe: the mechanisms that eliminated it were mutually reinforcing and offered no path to restore status within new frameworks. A single mechanism (law alone) would have permitted underground dueling or legal workarounds; the convergence of all four eliminated even that. The engine should classify this as tangled_rope precisely because the beneficiary narrative is true (coordination function exists: multiple paths to justice) AND the extraction is real (all honor-culture paths were closed simultaneously). The tension is not resolvable as false summit (natural law) or pure snare (no coordination); it is tangled because the coordination and extraction are operationally inseparable.
 *
 * DIRECTIONALITY LOGIC:
 *   Institutional legal reformers and nation-state consolidators are agenda-setters: they authored law and prosecution and hold enforcement power. Industrial bourgeoisie and cultural advocates are secondary beneficiaries: they benefit from dueling's decline but do not enforce it directly. Gentry, officers, and honor practitioners are full targets: every mechanism extracts from them in a different way (legal risk, procedural substitution, moral shame, military irrelevance), and none offers a path to maintain their status. Directionality for payers is near 1.0 (full targets); for beneficiaries it ranges from 0.15 (analytical reformers with no extraction) to 0.35 (state consolidators who bear enforcement costs). The identity_locked exit for honor practitioners reflects that accepting alternative remedy (law, credentials, wealth) required abandoning the identity (honor-culture membership) that made them who they were.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (how to answer insult in honor-based social order) had a live status through the 1850s because dueling persisted in practice despite legal prohibition. The founding problem became dead after the Civil War because the gentry class—the seat that had defined the problem—was destroyed, and the industrial, urban, professional-class order that replaced it had no need for dueling: insults were answered through libel law, duels between merchants were prosecuted with full force, and military honor came to depend on rank and efficiency rather than demonstrated courage. The constraint's mandatrophy is real and measurable: the founding problem is dead, the foundational dispute-settlement mechanism (dueling) is gone, but the constraint persists because it is now embedded in civil and military law. The law against dueling is no longer enforcing against a live practice (no one duels) but is rather maintaining a dead prohibition as a symbol of state monopoly on violence. Theater ratio rising from 0.28 to 0.51 models this: enforcement becomes increasingly theatrical (ceremonial prosecution of rare cases) as the practice itself dies. This is piton-adjacent behavior, but the constraint retains enough extractiveness from the historical moment (1790–1860) to be classified as tangled_rope during its active period rather than piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    causal_separability,
    'Were the four mechanisms (law, institutions, culture, war) operating as independent sufficient conditions, or were they structurally interdependent such that any single one would have failed without the others?',
    'Counterfactual historical analysis: examine jurisdictions where law was strong but cultural stigma was weak (some early German states with strict anti-dueling law but live honor culture), and jurisdictions where culture was strong but legal enforcement was lax (post-Napoleonic France with weak prosecution), to measure whether dueling persisted longer in the absence of any single mechanism.',
    'If mechanisms were truly independent sufficient conditions, dueling should have declined similarly across all four jurisdictional types. If mechanisms were interdependent, dueling should have persisted longer in jurisdictions lacking any one mechanism, and the measured ε should differ by jurisdiction. A finding of interdependence would strengthen the tangled_rope classification; independence would suggest decomposability into separate constraints (contraction vs. institutional vs. war mechanisms as distinct constraints).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(causal_separability, empirical, 'Whether the four decline mechanisms were causally separable or interdependent.').

omega_variable(
    beneficiary_primacy,
    'Which beneficiary set had the most causal agency in dueling''s decline: reformers (law), bourgeoisie (institutions), state (consolidation), or cultural advocates (narrative)? Did the constraint''s persistence depend on all four beneficiary sets coordinating, or did one drive the others?',
    'Political-economy analysis of legislative history, prosecution rates, cultural discourse timing, and post-Civil-War institutional changes across jurisdictions, to establish which mechanism preceded the others and which was most consistently funded and enforced.',
    'If one beneficiary set was primary and the others followed, the constraint is better understood as a snare (one mechanism with ideological cover) than as tangled_rope (multiple mechanisms). If all four beneficiary sets had independent motives and resources, tangled_rope classification holds. If the state was clearly primary (law and war), the constraint might be purely institutional extraction dressed in cultural language.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_primacy, empirical, 'Primacy of beneficiary sets in driving dueling''s decline.').

omega_variable(
    victim_agency,
    'Did honor-culture practitioners choose to abandon dueling in response to the convergent mechanisms, or were they coerced to abandon it? Was their ''exit'' from the constraint voluntary or forced?',
    'Literary and archival evidence of late duelists'' motivations, diary entries, prosecution decisions (execution vs. exile vs. imprisonment), and post-Civil-War officer narratives about whether they abandoned honor codes out of shame, fear, or recognition that the practice was unsustainable.',
    'If victims chose exit voluntarily (cultural narrative convinced them), the constraint is weaker (compliance is not coerced). If victims were forced (legal prosecution, military execution, social destruction after the Civil War), the constraint is stronger (coercion is real). If exit was mixed (some victims chose, some were forced), the extraction varies by victim subgroup and directionality is heterogeneous within the ''honor_culture_practitioners'' class.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_agency, empirical, 'Voluntariness of exit from honor-culture practices.').

omega_variable(
    reading_decomposition,
    'Is this overdetermined reading a true causal model, or is it a conceptual artifact of treating law, institutions, culture, and war as separable for analytical purposes when they are actually facets of a single historical transition from pre-industrial to industrial social order?',
    'Engagement with sibling readings (contraction and institutional_displacement): if each sibling can account for the observed decline independently when confounders are controlled for, then decomposition into separate readings is justified and each ε is interpretable. If sibling readings require auxiliary assumptions to explain the data, then the overdetermined reading is more parsimonious and decomposition is conceptual rather than causal.',
    'If valid: the framework correctly models multiple sufficient conditions as a tangled_rope constraint. If the reading is an artifact: dueling''s decline should be classified under one of the sibling readings (most likely contraction or institutional) and the overdetermined reading should be deprecated. This omega directly addresses the committer-frame tension: can the kernel be decomposed into independent readings, or is it a single phenomenon fractured into readings for analytical convenience?',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_decomposition, conceptual, 'Validity of causal decomposition in the overdetermined reading.').

omega_variable(
    civil_war_necessity,
    'Was the American Civil War a necessary condition for dueling''s decline in the United States, or would the legal, institutional, and cultural mechanisms alone have eliminated it within a generation?',
    'Comparison with European jurisdictions where dueling declined similarly (France, Prussia, Spain) without comparable civil war trauma; measurement of the rate of decline and institutionalization of law before and after 1861 to establish whether the Civil War accelerated or merely consolidated an already-underway process.',
    'If war was necessary (dueling persisted in significant numbers in the South until the war destroyed the gentry class), then ε for the pre-war period (1790–1861) is lower and the constraint is primarily post-war (1861–1880). If war was acceleration rather than necessity, then the pre-war mechanisms were already dominant and the Civil War merely eliminated residual support. A finding that war was necessary would reduce the plausibility of the contraction and institutional readings (which omit war entirely) and strengthen tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(civil_war_necessity, empirical, 'Necessity of the Civil War for dueling''s decline in the United States.').

omega_variable(
    reading_relationship_alternative,
    'Does this reading ''influence'' or ''forecloses'' the contraction and institutional_displacement readings, or do all three coexist as equally valid but differently-scoped models?',
    'Analytical: if the overdetermined reading claims that culture alone (contraction) or institutions alone (institutional_displacement) were insufficient without the others, then it forecloses those readings as complete explanations. If it claims that culture and institutions were both present and operative (but neither was uniquely causal), then it influences those readings by constraining their domains.',
    'Impacts the specification of cs_structure.reading_relations: if ''forecloses'', both sibling readings are logically incompatible with this reading within a single framework. If ''influences'', sibling readings remain live but are constrained to sub-domains (e.g., contraction applies in European context, institutional_displacement applies in American legal context). This uncertainty directly reflects OQ-109 Phase B ambiguity: is the kernel''s reading partition exhaustive and mutually exclusive, or are readings models that can coexist at different scopes?',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_relationship_alternative, conceptual, 'Logical relationship between overdetermined reading and its siblings in the kernel''s reading partition.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dueling_disappearance_mechanism__overdetermined_composite_reading, 0, 70).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(duel_tr_t0, dueling_disappearance_mechanism__overdetermined_composite_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(duel_tr_t0, observed).
narrative_ontology:measurement(duel_tr_t10, dueling_disappearance_mechanism__overdetermined_composite_reading, theater_ratio, 10, 0.32).
narrative_ontology:measurement_basis(duel_tr_t10, observed).
narrative_ontology:measurement(duel_tr_t20, dueling_disappearance_mechanism__overdetermined_composite_reading, theater_ratio, 20, 0.35).
narrative_ontology:measurement_basis(duel_tr_t20, observed).
narrative_ontology:measurement(duel_tr_t30, dueling_disappearance_mechanism__overdetermined_composite_reading, theater_ratio, 30, 0.39).
narrative_ontology:measurement_basis(duel_tr_t30, observed).
narrative_ontology:measurement(duel_tr_t45, dueling_disappearance_mechanism__overdetermined_composite_reading, theater_ratio, 45, 0.44).
narrative_ontology:measurement_basis(duel_tr_t45, observed).
narrative_ontology:measurement(duel_tr_t60, dueling_disappearance_mechanism__overdetermined_composite_reading, theater_ratio, 60, 0.48).
narrative_ontology:measurement_basis(duel_tr_t60, observed).
narrative_ontology:measurement(duel_tr_t70, dueling_disappearance_mechanism__overdetermined_composite_reading, theater_ratio, 70, 0.51).
narrative_ontology:measurement_basis(duel_tr_t70, observed).

% Extraction over time
narrative_ontology:measurement(duel_be_t0, dueling_disappearance_mechanism__overdetermined_composite_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement_basis(duel_be_t0, observed).
narrative_ontology:measurement(duel_be_t10, dueling_disappearance_mechanism__overdetermined_composite_reading, base_extractiveness, 10, 0.42).
narrative_ontology:measurement_basis(duel_be_t10, observed).
narrative_ontology:measurement(duel_be_t20, dueling_disappearance_mechanism__overdetermined_composite_reading, base_extractiveness, 20, 0.48).
narrative_ontology:measurement_basis(duel_be_t20, observed).
narrative_ontology:measurement(duel_be_t30, dueling_disappearance_mechanism__overdetermined_composite_reading, base_extractiveness, 30, 0.54).
narrative_ontology:measurement_basis(duel_be_t30, observed).
narrative_ontology:measurement(duel_be_t45, dueling_disappearance_mechanism__overdetermined_composite_reading, base_extractiveness, 45, 0.61).
narrative_ontology:measurement_basis(duel_be_t45, observed).
narrative_ontology:measurement(duel_be_t60, dueling_disappearance_mechanism__overdetermined_composite_reading, base_extractiveness, 60, 0.68).
narrative_ontology:measurement_basis(duel_be_t60, observed).
narrative_ontology:measurement(duel_be_t70, dueling_disappearance_mechanism__overdetermined_composite_reading, base_extractiveness, 70, 0.72).
narrative_ontology:measurement_basis(duel_be_t70, observed).

% Suppression requirement over time
narrative_ontology:measurement(duel_su_t0, dueling_disappearance_mechanism__overdetermined_composite_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement_basis(duel_su_t0, observed).
narrative_ontology:measurement(duel_su_t10, dueling_disappearance_mechanism__overdetermined_composite_reading, suppression_requirement, 10, 0.48).
narrative_ontology:measurement_basis(duel_su_t10, observed).
narrative_ontology:measurement(duel_su_t20, dueling_disappearance_mechanism__overdetermined_composite_reading, suppression_requirement, 20, 0.54).
narrative_ontology:measurement_basis(duel_su_t20, observed).
narrative_ontology:measurement(duel_su_t30, dueling_disappearance_mechanism__overdetermined_composite_reading, suppression_requirement, 30, 0.61).
narrative_ontology:measurement_basis(duel_su_t30, observed).
narrative_ontology:measurement(duel_su_t45, dueling_disappearance_mechanism__overdetermined_composite_reading, suppression_requirement, 45, 0.69).
narrative_ontology:measurement_basis(duel_su_t45, observed).
narrative_ontology:measurement(duel_su_t60, dueling_disappearance_mechanism__overdetermined_composite_reading, suppression_requirement, 60, 0.74).
narrative_ontology:measurement_basis(duel_su_t60, observed).
narrative_ontology:measurement(duel_su_t70, dueling_disappearance_mechanism__overdetermined_composite_reading, suppression_requirement, 70, 0.78).
narrative_ontology:measurement_basis(duel_su_t70, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=70
narrative_ontology:measurement(duel_grid_01, dueling_disappearance_mechanism__overdetermined_composite_reading, accessibility_collapse(class), 0, 0.48).
narrative_ontology:measurement(duel_grid_02, dueling_disappearance_mechanism__overdetermined_composite_reading, accessibility_collapse(class), 70, 0.74).
narrative_ontology:measurement(duel_grid_03, dueling_disappearance_mechanism__overdetermined_composite_reading, accessibility_collapse(individual), 0, 0.45).
narrative_ontology:measurement(duel_grid_04, dueling_disappearance_mechanism__overdetermined_composite_reading, accessibility_collapse(individual), 70, 0.78).
narrative_ontology:measurement(duel_grid_05, dueling_disappearance_mechanism__overdetermined_composite_reading, accessibility_collapse(organizational), 0, 0.52).
narrative_ontology:measurement(duel_grid_06, dueling_disappearance_mechanism__overdetermined_composite_reading, accessibility_collapse(organizational), 70, 0.81).
narrative_ontology:measurement(duel_grid_07, dueling_disappearance_mechanism__overdetermined_composite_reading, accessibility_collapse(structural), 0, 0.38).
narrative_ontology:measurement(duel_grid_08, dueling_disappearance_mechanism__overdetermined_composite_reading, accessibility_collapse(structural), 70, 0.68).
narrative_ontology:measurement(duel_grid_09, dueling_disappearance_mechanism__overdetermined_composite_reading, resistance(class), 0, 0.75).
narrative_ontology:measurement(duel_grid_10, dueling_disappearance_mechanism__overdetermined_composite_reading, resistance(class), 70, 0.25).
narrative_ontology:measurement(duel_grid_11, dueling_disappearance_mechanism__overdetermined_composite_reading, resistance(individual), 0, 0.72).
narrative_ontology:measurement(duel_grid_12, dueling_disappearance_mechanism__overdetermined_composite_reading, resistance(individual), 70, 0.28).
narrative_ontology:measurement(duel_grid_13, dueling_disappearance_mechanism__overdetermined_composite_reading, resistance(organizational), 0, 0.78).
narrative_ontology:measurement(duel_grid_14, dueling_disappearance_mechanism__overdetermined_composite_reading, resistance(organizational), 70, 0.22).
narrative_ontology:measurement(duel_grid_15, dueling_disappearance_mechanism__overdetermined_composite_reading, resistance(structural), 0, 0.68).
narrative_ontology:measurement(duel_grid_16, dueling_disappearance_mechanism__overdetermined_composite_reading, resistance(structural), 70, 0.35).
narrative_ontology:measurement(duel_grid_17, dueling_disappearance_mechanism__overdetermined_composite_reading, stakes_inflation(class), 0, 0.51).
narrative_ontology:measurement(duel_grid_18, dueling_disappearance_mechanism__overdetermined_composite_reading, stakes_inflation(class), 70, 0.79).
narrative_ontology:measurement(duel_grid_19, dueling_disappearance_mechanism__overdetermined_composite_reading, stakes_inflation(individual), 0, 0.52).
narrative_ontology:measurement(duel_grid_20, dueling_disappearance_mechanism__overdetermined_composite_reading, stakes_inflation(individual), 70, 0.82).
narrative_ontology:measurement(duel_grid_21, dueling_disappearance_mechanism__overdetermined_composite_reading, stakes_inflation(organizational), 0, 0.58).
narrative_ontology:measurement(duel_grid_22, dueling_disappearance_mechanism__overdetermined_composite_reading, stakes_inflation(organizational), 70, 0.86).
narrative_ontology:measurement(duel_grid_23, dueling_disappearance_mechanism__overdetermined_composite_reading, stakes_inflation(structural), 0, 0.42).
narrative_ontology:measurement(duel_grid_24, dueling_disappearance_mechanism__overdetermined_composite_reading, stakes_inflation(structural), 70, 0.72).
narrative_ontology:measurement(duel_grid_25, dueling_disappearance_mechanism__overdetermined_composite_reading, suppression(class), 0, 0.42).
narrative_ontology:measurement(duel_grid_26, dueling_disappearance_mechanism__overdetermined_composite_reading, suppression(class), 70, 0.79).
narrative_ontology:measurement(duel_grid_27, dueling_disappearance_mechanism__overdetermined_composite_reading, suppression(individual), 0, 0.38).
narrative_ontology:measurement(duel_grid_28, dueling_disappearance_mechanism__overdetermined_composite_reading, suppression(individual), 70, 0.76).
narrative_ontology:measurement(duel_grid_29, dueling_disappearance_mechanism__overdetermined_composite_reading, suppression(organizational), 0, 0.48).
narrative_ontology:measurement(duel_grid_30, dueling_disappearance_mechanism__overdetermined_composite_reading, suppression(organizational), 70, 0.84).
narrative_ontology:measurement(duel_grid_31, dueling_disappearance_mechanism__overdetermined_composite_reading, suppression(structural), 0, 0.35).
narrative_ontology:measurement(duel_grid_32, dueling_disappearance_mechanism__overdetermined_composite_reading, suppression(structural), 70, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dueling_disappearance_mechanism__overdetermined_composite_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(dueling_disappearance_mechanism__overdetermined_composite_reading, 0.12).
narrative_ontology:affects_constraint(dueling_disappearance_mechanism__overdetermined_composite_reading, dueling_disappearance_mechanism__contraction_reading).
narrative_ontology:affects_constraint(dueling_disappearance_mechanism__overdetermined_composite_reading, dueling_disappearance_mechanism__institutional_displacement_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the kernel 'dueling_disappearance_mechanism'. The kernel contest is whether dueling's decline was caused by cultural shift (contraction_reading), institutional substitution (institutional_displacement_reading), or causal overdetermination (overdetermined_composite_reading, this story). The three readings have different ε values: the contraction reading assumes ε is culture's leverage on honor identity (~0.45, coordination function is strong, suppression is low); the institutional reading assumes ε is institutional efficiency (~0.52, coordination function is efficiency, suppression is moderate); the overdetermined reading assumes ε is non-decomposable (~0.58, no single mechanism isolates the effect). Each reading is a constraint family member and must be authored separately with its own stakeholders, mechanisms, and beneficiary/victim structures. This reading claims none of the sibling mechanisms was individually sufficient; the others claim sufficiency for their respective mechanisms. The network edges above indicate downstream influence: the overdetermined reading's claim of interdependence affects how the sibling readings can be interpreted (either as valid for sub-domains or as incomplete models of a single historical transition).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(dueling_disappearance_mechanism__overdetermined_composite_reading, organized, 0.28).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
