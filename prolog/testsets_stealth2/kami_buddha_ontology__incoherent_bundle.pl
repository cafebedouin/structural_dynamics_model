% ============================================================================
% CONSTRAINT STORY: kami_buddha_ontology__incoherent_bundle
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [HISTORICAL — DISSOLVED BY MEIJI SEPARATION EDICTS (1868)]
% ============================================================================

:- module(constraint_kami_buddha_ontology__incoherent_bundle, []).

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
 *   constraint_id: kami_buddha_ontology__incoherent_bundle
 *   human_readable: Shinbutsu-shugo as Institutionally Sustained Contradiction Bundle
 *   domain: religious_studies/japanese_cultural_history
 *
 * SUMMARY:
 *   Shinbutsu-shugo — the millennium-long fusion of kami worship and Buddhism
 *   in Japan — is modeled here as the standing arrangement under contest,
 *   read through the incoherent_bundle reading: not one ontology but a bundle
 *   of contradictory commitments (simultaneous fusion and separation,
 *   hierarchical subordination and reciprocal exchange, systematizing
 *   treatise and unsystematizable practice) held together by institutional
 *   inertia and ritual success. The interval runs t0 = 1168 CE to t70 = 1868
 *   CE, one time unit = ten years, terminating at the Meiji separation
 *   edicts. Across the interval the arrangement delivers real coordination
 *   goods (cult integration, mortuary care, ritual legitimation) while
 *   accumulating extraction (temple-shrine estates, then compulsory
 *   parishioner affiliation with a funerary-fee monopoly) and enforcement
 *   (from monastic militancy to bakufu certification police). Per the
 *   epsilon-invariance principle this is ONE of three readings of the kernel,
 *   each a separate file with its own epsilon; the siblings are linked in
 *   network.affects_constraints. KEY AGENTS (by structural relationship): -
 *   buddhist_monastic_establishments: Agenda-setting beneficiary
 *   (institutional/arbitrage) — administers the fused complex, collects its
 *   revenues, repositions doctrine when politics shifts - tokugawa_bakufu:
 *   Late-interval agenda-setter (institutional/arbitrage) — converts the
 *   bundle into a certification-police instrument - imperial_court:
 *   Beneficiary (institutional/identity_locked) — its sacral legitimacy is
 *   constituted by the fused order - shrine_priestly_lineages: Beneficiary
 *   with accumulating grievance (organized/identity_locked) — collects rank
 *   and stipend through the very subordination it resents -
 *   rural_household_parishioners: Primary target (powerless/trapped) —
 *   compulsory affiliation, funerary fees, certification rolls -
 *   village_kami_cult_communities: Primary target (organized/trapped) —
 *   autonomous cult subordinated to temple orbit -
 *   women_under_purity_exclusions: Primary target (powerless/trapped) — fused
 *   pollution doctrine bars and bills them - doctrinal_systematizers: Target
 *   with side-payments (moderate/constrained) — absorbs the labor of holding
 *   contradictions together - nativist_scholars: Excluded voice
 *   (organized/constrained) — objects from outside, censored, eventually
 *   decisive - religious_studies_historians: Analytical observer
 *   (analytical/analytical) — sees the bundle, bears nothing
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kami_buddha_ontology__incoherent_bundle, 0.78).
domain_priors:suppression_score(kami_buddha_ontology__incoherent_bundle, 0.82).
domain_priors:theater_ratio(kami_buddha_ontology__incoherent_bundle, 0.66).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kami_buddha_ontology__incoherent_bundle, extractiveness, 0.78).
narrative_ontology:constraint_metric(kami_buddha_ontology__incoherent_bundle, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(kami_buddha_ontology__incoherent_bundle, theater_ratio, 0.66).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kami_buddha_ontology__incoherent_bundle, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(kami_buddha_ontology__incoherent_bundle, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kami_buddha_ontology__incoherent_bundle, tangled_rope).
narrative_ontology:human_readable(kami_buddha_ontology__incoherent_bundle, "Shinbutsu-shugo as Institutionally Sustained Contradiction Bundle").
narrative_ontology:topic_domain(kami_buddha_ontology__incoherent_bundle, "religious_studies/japanese_cultural_history").

domain_priors:requires_active_enforcement(kami_buddha_ontology__incoherent_bundle).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kami_buddha_ontology__incoherent_bundle, '886a475b-83cc-4525-9612-db34de02f5e7').
narrative_ontology:cs_kernel_codification('886a475b-83cc-4525-9612-db34de02f5e7', distributed).
narrative_ontology:cs_authority_grounding('886a475b-83cc-4525-9612-db34de02f5e7', distributed).
narrative_ontology:cs_reading_relation('886a475b-83cc-4525-9612-db34de02f5e7', kami_buddha_ontology__honji_suijaku_monism, coexists_with).
narrative_ontology:cs_reading_relation('886a475b-83cc-4525-9612-db34de02f5e7', kami_buddha_ontology__domain_partition, coexists_with).
narrative_ontology:cs_axiom('886a475b-83cc-4525-9612-db34de02f5e7', foundational, no_single_ontology_governs_fusion).
narrative_ontology:cs_axiom_status(no_single_ontology_governs_fusion, holdable).
narrative_ontology:cs_axiom_grounding('886a475b-83cc-4525-9612-db34de02f5e7', no_single_ontology_governs_fusion, empirically_contingent).
narrative_ontology:cs_axiom('886a475b-83cc-4525-9612-db34de02f5e7', foundational, institutional_inertia_sustains_contradiction).
narrative_ontology:cs_axiom_status(institutional_inertia_sustains_contradiction, holdable).
narrative_ontology:cs_axiom_grounding('886a475b-83cc-4525-9612-db34de02f5e7', institutional_inertia_sustains_contradiction, empirically_contingent).
narrative_ontology:cs_reference_frame('886a475b-83cc-4525-9612-db34de02f5e7', practical_pluralism_baseline).
narrative_ontology:cs_drift_state('886a475b-83cc-4525-9612-db34de02f5e7', meiji_separation_edicts, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('886a475b-83cc-4525-9612-db34de02f5e7', '2026-08-05T00:00:00Z').
narrative_ontology:cs_kernel_id(kami_buddha_ontology__incoherent_bundle, kami_buddha_ontology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kami_buddha_ontology__incoherent_bundle, buddhist_monastic_establishments).
narrative_ontology:constraint_beneficiary(kami_buddha_ontology__incoherent_bundle, shrine_priestly_lineages).
narrative_ontology:constraint_beneficiary(kami_buddha_ontology__incoherent_bundle, imperial_court).
narrative_ontology:constraint_beneficiary(kami_buddha_ontology__incoherent_bundle, tokugawa_bakufu).
narrative_ontology:constraint_victim(kami_buddha_ontology__incoherent_bundle, rural_household_parishioners).
narrative_ontology:constraint_victim(kami_buddha_ontology__incoherent_bundle, village_kami_cult_communities).
narrative_ontology:constraint_victim(kami_buddha_ontology__incoherent_bundle, women_under_purity_exclusions).
narrative_ontology:constraint_victim(kami_buddha_ontology__incoherent_bundle, doctrinal_systematizers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(kami_buddha_ontology__incoherent_bundle, doctrinal_systematizers).
narrative_ontology:constraint_victim(kami_buddha_ontology__incoherent_bundle, shrine_priestly_lineages).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate the fused complex day to day: station monks at shrines as supervising clergy, perform Buddhist rites over kami festivals, hold the doctrinal schools that supply trace-and-ground readings, and collect land rent, funerary fees, and parishioner dues across temple-shrine networks. When subordination of kami becomes politically costly, they can reposition doctrinally — reversing the hierarchy so kami become the original ground — without surrendering institutional control.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__incoherent_bundle, buddhist_monastic_establishments, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(kami_buddha_ontology__incoherent_bundle, buddhist_monastic_establishments, beneficiary).

% From the seventeenth century administers the whole complex through temple laws and the parishioner-certification system: every household must register with a Buddhist temple, temples vouch for their parishioners annually, and village headmen answer collectively for noncompliance. Gains a census-like surveillance instrument and leverage over the religious orders, and can restructure the arrangement by edict — a power the Meiji government that replaces it exercises in 1868.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__incoherent_bundle, tokugawa_bakufu, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(kami_buddha_ontology__incoherent_bundle, tokugawa_bakufu, beneficiary).

% Grants ranks and titles that bind shrine and temple into one ritual order presided over by the sovereign. The emperor's own sacral identity is double — descendant of the sun kami and figure of Buddhist protector-deity lore — so the fused order is constitutive of imperial legitimacy rather than a policy the court could trade away.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__incoherent_bundle, imperial_court, beneficiary,
    institutional, civilizational, identity_locked, national).

% Hereditary shrine-priest houses receive court rank, stipends, and doctrinal sanction through affiliation with Buddhist establishments; their rites, calendars, and even their ancestral myths are reframed in Buddhist terms by the schools they serve. Repudiating the fusion would mean repudiating the ancestral offices, esoteric transmissions, and ranked status that constitute the lineage itself, so exit is unthinkable even where resentment accumulates.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__incoherent_bundle, shrine_priestly_lineages, beneficiary,
    organized, generational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(kami_buddha_ontology__incoherent_bundle, shrine_priestly_lineages, payer).

% Every household must hold membership at a Buddhist temple, pay for funerals and memorial services at temple-set rates, and appear in annual certification rolls; refusal marks the household as Christian or criminal and exposes the whole village headman group to punishment. Individual exit means social death, and the obligation reproduces down the generations.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__incoherent_bundle, rural_household_parishioners, payer,
    powerless, biographical, trapped, national).

% Village assemblies maintain the local kami cult — festivals, shrine upkeep, processions — but the shrine sits inside a temple's orbit: its lands may be held as temple estate, its priest supervised by resident monks, its deity narrated as a manifestation of a buddha. Communities sometimes resist through litigation and uprising, but cannot dissolve the fusion without dissolving the cult's own legitimacy.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__incoherent_bundle, village_kami_cult_communities, payer,
    organized, generational, trapped, local).

% Fused pollution doctrine treats childbirth and menstruation as impurity requiring paid purification and bars women from specified sacred mountains and inner sanctums; the exclusion regime tightens as Buddhist death-pollution teaching merges with kami purity taboo. There is no parallel sacred geography open to women outside the fused complex.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__incoherent_bundle, women_under_purity_exclusions, payer,
    powerless, biographical, trapped, national).

% Scholar-monks produce the treatises that reconcile kami worship with Buddhist doctrine — subordination schemas, trace-and-ground mappings, reversed hierarchies. The work is endless because the commitments refuse unification: each synthesis leaves residues that the next controversy exposes. They gain school standing and patronage from the effort while absorbing the intellectual cost of holding the contradictions together.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__incoherent_bundle, doctrinal_systematizers, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(kami_buddha_ontology__incoherent_bundle, doctrinal_systematizers, beneficiary).

% Kokugaku philologists argue that kami worship predates and outranks Buddhist borrowing and that the fused corpus corrupts the originals. Their teaching circulates through private academies under censorship pressure and is barred from official curricula, yet their students staff the movement that dismantles the arrangement in 1868.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__incoherent_bundle, nativist_scholars, excluded,
    organized, generational, constrained, national).

% Modern historiography reconstructs the arrangement from temple registers, edict archives, and village documents, testing whether any single doctrinal formulation accounts for the practice record. It holds no position inside the arrangement and bears none of its costs.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__incoherent_bundle, religious_studies_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(kami_buddha_ontology__incoherent_bundle, buddhist_monastic_establishments).
narrative_ontology:fixing_cost_class(kami_buddha_ontology__incoherent_bundle, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Integrates two cult systems into one legible ritual order: shared sites, shared festival calendars, doctrinally sanctioned kami cults, literate mortuary care for households, and a single hierarchy of ritual legitimation reaching from village shrine to court.
% TRANSFER_FUNCTION: Moves material support — land rent, rice levies, funerary and memorial fees, parishioner dues — from rural households and village communities to temple-shrine complexes; moves ritual legitimacy downward (court rank and doctrinal sanction to shrines) and cult allegiance upward (local deities enrolled under central institutions); moves death-handling labor from families to temples.
% ABSENT_VOICES: Nativist scholars objecting to the subordination of kami were excluded from official doctrinal councils and censored; village communities bearing dual shrine and temple obligations had no seat where fees and calendars were set; women subject to the fused purity regime had no voice in its rule-making; hidden Christians faced extermination-level exclusion with no forum at all.
% DISAPPEARANCE_RATIONALE: Removal rearranged the world violently: when the Meiji state severed kami and buddhas in 1868, thousands of shrine-temples were demolished, Buddhist objects burned or desecrated in the haibutsu kishaku wave, shrine priests forced to laicize or requalify, parishioner obligations voided overnight, and village ritual calendars rewritten — demonstrating that a dense web of offices, revenues, and identities depended on the arrangement.
% FOUNDING_PROBLEM: Make an indigenous cult landscape commensurable with an imported salvative religion arriving with writing, statecraft, and medicine — so that kami and buddhas could share sites, calendars, and patrons without either cult collapsing.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: kokugaku philologists (Motoori Norinaga, Hirata Atsutane) attested that the commensuration problem was long solved and the fusion sustained only by institutional interest; Confucian advisers to domain governments documented the doctrine's internal contradictions; modern historiography (Kuroda Toshio's reconstruction of the kenmitsu order) independently concludes the arrangement persisted through the interests of its administrators rather than any live doctrinal need. Defending parties (Ryobu and Shingon traditions) attest continuing soteriological function, hence the contested status; no benefiting party's attestation is relied upon.
narrative_ontology:disappearance_verdict(kami_buddha_ontology__incoherent_bundle, world_rearranges).
narrative_ontology:founding_problem_status(kami_buddha_ontology__incoherent_bundle, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kami_buddha_ontology__incoherent_bundle, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(kami_buddha_ontology__incoherent_bundle, 'none', 1).
narrative_ontology:epsilon_provenance(kami_buddha_ontology__incoherent_bundle, 0.78, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kami_buddha_ontology__incoherent_bundle_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(kami_buddha_ontology__incoherent_bundle, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(kami_buddha_ontology__incoherent_bundle_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored high (0.78 at interval end) because the late arrangement compounds land rent, compulsory parishioner affiliation, and a funerary-services monopoly priced above household discretion. Suppression (0.82) is authored as a raw structural property — unscaled by power or scope — reflecting the certification-and-collective-responsibility machinery of the later interval; extractiveness, by contrast, is what the engine scales by directionality and scope. Theater (0.66) reflects doctrinal production increasingly defending institutional position rather than articulating practice, crossing the Goodhart threshold around t50. Accessibility collapse (0.6) is partial: exclusive-kami devotion and hidden-Christian practice survived at the margins but at high cost. Resistance (0.55) reflects real coalition capacity — village litigation, ikki uprisings, nativist mobilization — met by the enforcement apparatus. Claim and metrics are independent: the claim is tangled_rope because both coordination and extraction are structurally real and actively enforced; the metric series additionally records drift toward snare-flavored operation across the interval, which the engine weighs on its own terms. The extractiveness dip at t30-t40 is an exogenous Sengoku-warfare shock to collection capacity, not an oscillatory extraction mechanism. Coordination type is identity_coordination: the dominant function is boundary and membership coordination between two cult systems; the FNL gaming risk is acknowledged — extraction concentrates on powerless agents at national scope, which the coupling test should scrutinize rather than excuse via the complexity offset. All three series share one decade grid (every tracked metric authored at every point t0-t70).
 *
 * PERSPECTIVAL GAP:
 *   Seats compute differently. From the monastic agenda-setter seat the arrangement is a legitimate order it administers and doctrinally defends — rope-flavored. From the trapped parishioner and village-cult seats the same structure operates as compulsory extraction — snare-flavored. The identity-locked shrine lineages and imperial court experience the arrangement as constitutive rather than chosen: exit would dissolve the lineage or the throne's own sacral identity, so neither cost nor benefit is experienced as transactional. The doctrinal systematizer seat experiences a distinctive burden — the arrangement's contradictions are that seat's daily work. The analytical observer seat sees no ontology at all, only managed contradiction. The engine derives these divergences from power, exit, and role data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations place the monastic establishments, court, bakufu, and shrine lineages near the subsidized end; victim declarations place households, village cults, women, and systematizers near the target end. Exit modulation sharpens this: trapped parishioners and cult communities sit at the full-target end, while arbitrage-capable administrators sit nearest the beneficiary end. One override: doctrinal_systematizers carry both payer and beneficiary roles, and the derivation would average their dual position toward symmetry; their actual burden — endless reconciliation labor and career exposure when syntheses collapse — places them nearer the target end, so d is overridden to 0.6 for the moderate power atom, which in this story contains only that seat (nativist_scholars are carried at organized, keeping the override surgical).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — making an indigenous cult landscape commensurable with an imported salvative religion — was substantively solved by the twelfth century, when shared sites, calendars, and doctrinal vocabularies were routine. What persisted for seven more centuries was the bundle of offices and revenues the solution had crystallized into. Reading the arrangement as pure coordination would miss the funerary-monopoly rents and compulsory affiliation of the later interval; reading it as pure extraction would miss the real ritual goods delivered across its first half. The tangled_rope claim with rising theater and suppression tracks the transition from live coordination to interest-maintained performance, and the R5 mismatch (dead-or-contested founding problem, world-rearranging persistence) flags the zombie phase the mismatch consumer is designed to catch. Mandatrophy is declared resolved: the mandate outlived its function centuries before the arrangement itself was dissolved.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'Is the standing shinbutsu-shugo arrangement one coherent ontological settlement (as the honji_suijaku_monism and domain_partition readings hold, differently) or an institutionally sustained bundle of contradictory commitments with no governing ontology (this reading)?',
    'Systematic comparison of doctrinal corpora against practice records: if any single formulation (trace-and-ground identity, or functional partition) accounts for both the subordination schemas and the reciprocal shrine-temple exchanges without residue, a sibling reading absorbs this one; if every formulation leaves documented residues that later controversies exploit, the bundle reading stands.',
    'If a sibling''s ontology suffices, this constraint collapses into that sibling and epsilon redistributes to the sibling''s beneficiary/victim structure; if not, classification keys on enforcement holding contradictions in place rather than on any doctrine''s truth.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Whether the fusion constitutes one ontology or a managed contradiction bundle.').

omega_variable(
    incoherence_intrinsic_or_strategic,
    'Is the arrangement''s incoherence intrinsic — no framework could unify the commitments — or strategic, with unification possible but institutionally disincentivized because each contradiction serves some seat?',
    'Counterfactual doctrinal history: examine episodes where unification was attempted (Yoshida reverse hierarchy, Edo-period rationalist critiques) and determine whether failure followed from logical impossibility or from interested obstruction.',
    'Intrinsic incoherence pushes the arrangement toward structural-inevitability readings; strategic incoherence strengthens the extraction reading, since each contradiction then functions as a rent-preserving device.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(incoherence_intrinsic_or_strategic, conceptual, 'Source of the bundle''s irresolution.').

omega_variable(
    suppression_driver_ambiguity,
    'Is the late-interval enforcement ratchet (parishioner certification, collective responsibility) driven by the anti-Christian security problem external to the bundle, or by the bundle''s own need to suppress exits from its revenue base?',
    'Compare enforcement intensity against measured Christian presence after the Shimabara repression: if certification machinery keeps expanding after the external threat is extinguished, the driver is internal rent-defense.',
    'An external driver would credit part of the measured suppression to state security rather than the constraint, lowering the bundle''s intrinsic suppression; an internal driver confirms the enforcement ratchet as the bundle''s own maintenance cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_driver_ambiguity, empirical, 'What the late enforcement machinery was actually for.').

omega_variable(
    efficacy_vs_enforcement_persistence,
    'Could the bundle have persisted on practical efficacy alone — ritual goods, mortuary care, cult integration — absent compulsion, or was coercion load-bearing throughout?',
    'Regional comparisons of domains and periods with lax versus rigorous enforcement, tracking voluntary affiliation rates, festival continuity, and shrine-temple cooperation.',
    'If efficacy alone sustains participation, the coordination component outweighs extraction and the arrangement reads closer to rope; if participation tracks enforcement intensity, the extraction component dominates.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(efficacy_vs_enforcement_persistence, empirical, 'Relative weight of efficacy and coercion in persistence.').

omega_variable(
    separation_failure_scope,
    'Does the 1868 success of shinbutsu bunri — achieved only by a revolutionary regime destroying the entire Tokugawa order — confirm or qualify the reading''s claim that separation attempts fail?',
    'Classify pre-1868 separation attempts (shrine purgations, domain-level edicts, nativist petitions) by outcome, and model the Meiji case as regime-change rather than intra-arrangement correction.',
    'If all intra-arrangement attempts failed and only extra-arrangement revolution succeeded, the reading''s claim holds with a boundary condition; if some intra-arrangement attempt nearly succeeded, the bundle was less locked than claimed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(separation_failure_scope, conceptual, 'Boundary conditions on the separation-failure claim.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kami_buddha_ontology__incoherent_bundle, 0, 70).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(kami_buddha_bundle_tr_t0, kami_buddha_ontology__incoherent_bundle, theater_ratio, 0, 0.25).
narrative_ontology:measurement(kami_buddha_bundle_tr_t10, kami_buddha_ontology__incoherent_bundle, theater_ratio, 10, 0.28).
narrative_ontology:measurement(kami_buddha_bundle_tr_t20, kami_buddha_ontology__incoherent_bundle, theater_ratio, 20, 0.33).
narrative_ontology:measurement(kami_buddha_bundle_tr_t30, kami_buddha_ontology__incoherent_bundle, theater_ratio, 30, 0.38).
narrative_ontology:measurement(kami_buddha_bundle_tr_t40, kami_buddha_ontology__incoherent_bundle, theater_ratio, 40, 0.42).
narrative_ontology:measurement(kami_buddha_bundle_tr_t50, kami_buddha_ontology__incoherent_bundle, theater_ratio, 50, 0.5).
narrative_ontology:measurement(kami_buddha_bundle_tr_t60, kami_buddha_ontology__incoherent_bundle, theater_ratio, 60, 0.58).
narrative_ontology:measurement(kami_buddha_bundle_tr_t70, kami_buddha_ontology__incoherent_bundle, theater_ratio, 70, 0.66).

% Extraction over time
narrative_ontology:measurement(kami_buddha_bundle_be_t0, kami_buddha_ontology__incoherent_bundle, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(kami_buddha_bundle_be_t10, kami_buddha_ontology__incoherent_bundle, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(kami_buddha_bundle_be_t20, kami_buddha_ontology__incoherent_bundle, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(kami_buddha_bundle_be_t30, kami_buddha_ontology__incoherent_bundle, base_extractiveness, 30, 0.55).
narrative_ontology:measurement(kami_buddha_bundle_be_t40, kami_buddha_ontology__incoherent_bundle, base_extractiveness, 40, 0.48).
narrative_ontology:measurement(kami_buddha_bundle_be_t50, kami_buddha_ontology__incoherent_bundle, base_extractiveness, 50, 0.62).
narrative_ontology:measurement(kami_buddha_bundle_be_t60, kami_buddha_ontology__incoherent_bundle, base_extractiveness, 60, 0.72).
narrative_ontology:measurement(kami_buddha_bundle_be_t70, kami_buddha_ontology__incoherent_bundle, base_extractiveness, 70, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(kami_buddha_bundle_su_t0, kami_buddha_ontology__incoherent_bundle, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(kami_buddha_bundle_su_t10, kami_buddha_ontology__incoherent_bundle, suppression_requirement, 10, 0.26).
narrative_ontology:measurement(kami_buddha_bundle_su_t20, kami_buddha_ontology__incoherent_bundle, suppression_requirement, 20, 0.34).
narrative_ontology:measurement(kami_buddha_bundle_su_t30, kami_buddha_ontology__incoherent_bundle, suppression_requirement, 30, 0.4).
narrative_ontology:measurement(kami_buddha_bundle_su_t40, kami_buddha_ontology__incoherent_bundle, suppression_requirement, 40, 0.45).
narrative_ontology:measurement(kami_buddha_bundle_su_t50, kami_buddha_ontology__incoherent_bundle, suppression_requirement, 50, 0.6).
narrative_ontology:measurement(kami_buddha_bundle_su_t60, kami_buddha_ontology__incoherent_bundle, suppression_requirement, 60, 0.7).
narrative_ontology:measurement(kami_buddha_bundle_su_t70, kami_buddha_ontology__incoherent_bundle, suppression_requirement, 70, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kami_buddha_ontology__incoherent_bundle, identity_coordination).
narrative_ontology:affects_constraint(kami_buddha_ontology__incoherent_bundle, kami_buddha_ontology__honji_suijaku_monism).
narrative_ontology:affects_constraint(kami_buddha_ontology__incoherent_bundle, kami_buddha_ontology__domain_partition).

% DUAL FORMULATION NOTE:
% Family decomposition per the epsilon-invariance principle: the colloquial label 'shinbutsu-shugo' covers three structurally distinct claims — (1) ontological identity of kami and buddhas (honji_suijaku_monism), (2) functional partition between distinct entities (domain_partition), (3) no governing ontology, only an institutionally sustained contradiction bundle (this file). Each carries its own epsilon, beneficiaries, and victims. Upstream monism historically supplied the doctrinal vocabulary the other two readings react to, hence the edges. This reading's epsilon refers to the standing arrangement assessed as a contradiction bundle — not to either sibling's proposed ontology.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(kami_buddha_ontology__incoherent_bundle, moderate, 0.6).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
