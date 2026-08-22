% ============================================================================
% CONSTRAINT STORY: imperial_mandate__loyalist_restoration_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-18
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
 *   constraint_id: imperial_mandate__loyalist_restoration_reading
 *   human_readable: Loyalist Restoration Reading: Divine Mandate as Unmediated Imperial Sovereignty
 *   domain: political_philosophy/comparative_constitutional_systems/east_asian_history
 *
 * SUMMARY:
 *   This constraint instantiates the loyalist-restoration reading of the
 *   imperial mandate kernel: the claim that legitimate sovereignty in Japan
 *   requires the emperor's unmediated, active exercise of governance, and
 *   that any intermediary structure — the Tokugawa shogunate, the hereditary
 *   samurai administrative class — constitutes usurpation regardless of its
 *   actual governing competence. This reading drove the ideological
 *   mobilization of the Boshin War and the subsequent dismantling of the han
 *   system, culminating in the Meiji Constitution's formal (if ultimately
 *   oligarchically mediated) vesting of sovereignty in the throne. The
 *   sibling reading, bakufu_delegation_reading, holds that the mandate's
 *   legitimacy-granting function is separable from active governance and that
 *   delegated rule under shogunal administration was fully compatible with
 *   the emperor's sacred status. These are not two measurements of the same
 *   constraint — they are two different constraints with different
 *   beneficiary sets, different victim sets, and different ε: this reading
 *   authors substantial and rising extraction (as the doctrine hardens from
 *   persuasive claim into enforced constitutional settlement), while the
 *   delegation reading would author a low, stable ε consistent with centuries
 *   of uncontested co-existence between ritual and administrative
 *   sovereignty.
 *
 * KEY AGENTS:
 *   - loyalist_domain_factions: Primary agenda-setter and beneficiary (organized/mobile) — administers the doctrine's political-military campaign and converts service into post-restoration power
 *   - sonno_joi_ideologues: Doctrinal architects (organized/identity_locked) — construct the textual legitimacy claim and are professionally and personally fused to its success
 *   - shogunal_bureaucracy: Primary target (institutional/trapped) — entire governing function retroactively delegitimized regardless of competence
 *   - hereditary_samurai_retainer_class: Secondary target (moderate/constrained) — status and livelihood collapse as intermediary structures are dismantled
 *   - constitutional_historians: Analytical observer — assesses whether restoration was genuine recovery or retrospective invention
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(imperial_mandate__loyalist_restoration_reading, 0.58).
domain_priors:suppression_score(imperial_mandate__loyalist_restoration_reading, 0.71).
domain_priors:theater_ratio(imperial_mandate__loyalist_restoration_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(imperial_mandate__loyalist_restoration_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(imperial_mandate__loyalist_restoration_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(imperial_mandate__loyalist_restoration_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(imperial_mandate__loyalist_restoration_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(imperial_mandate__loyalist_restoration_reading, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(imperial_mandate__loyalist_restoration_reading, tangled_rope).
narrative_ontology:human_readable(imperial_mandate__loyalist_restoration_reading, "Loyalist Restoration Reading: Divine Mandate as Unmediated Imperial Sovereignty").
narrative_ontology:topic_domain(imperial_mandate__loyalist_restoration_reading, "political_philosophy/comparative_constitutional_systems/east_asian_history").

domain_priors:requires_active_enforcement(imperial_mandate__loyalist_restoration_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(imperial_mandate__loyalist_restoration_reading, '1b03627c-e3f4-48fc-8f3c-5d5bdd87e9f9').
narrative_ontology:cs_kernel_codification('1b03627c-e3f4-48fc-8f3c-5d5bdd87e9f9', distributed).
narrative_ontology:cs_authority_grounding('1b03627c-e3f4-48fc-8f3c-5d5bdd87e9f9', lineage).
narrative_ontology:cs_interpretation_layer_present('1b03627c-e3f4-48fc-8f3c-5d5bdd87e9f9').
narrative_ontology:cs_reading_relation('1b03627c-e3f4-48fc-8f3c-5d5bdd87e9f9', imperial_mandate__bakufu_delegation_reading, forecloses).
narrative_ontology:cs_axiom('1b03627c-e3f4-48fc-8f3c-5d5bdd87e9f9', foundational, sovereignty_and_legitimacy_are_inseparable).
narrative_ontology:cs_axiom_status(sovereignty_and_legitimacy_are_inseparable, holdable).
narrative_ontology:cs_axiom_grounding('1b03627c-e3f4-48fc-8f3c-5d5bdd87e9f9', sovereignty_and_legitimacy_are_inseparable, deontological).
narrative_ontology:cs_axiom('1b03627c-e3f4-48fc-8f3c-5d5bdd87e9f9', secondary, intermediary_governance_constitutes_usurpation).
narrative_ontology:cs_axiom_status(intermediary_governance_constitutes_usurpation, holdable).
narrative_ontology:cs_axiom_grounding('1b03627c-e3f4-48fc-8f3c-5d5bdd87e9f9', intermediary_governance_constitutes_usurpation, conventional).
narrative_ontology:cs_reference_frame('1b03627c-e3f4-48fc-8f3c-5d5bdd87e9f9', unmediated_imperial_sovereignty_precedent).
narrative_ontology:cs_drift_state('1b03627c-e3f4-48fc-8f3c-5d5bdd87e9f9', boshin_restoration_settlement, gap(revival_pressure, severe, true)).
narrative_ontology:cs_created_at('1b03627c-e3f4-48fc-8f3c-5d5bdd87e9f9', '').
narrative_ontology:cs_kernel_id(imperial_mandate__loyalist_restoration_reading, imperial_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(imperial_mandate__loyalist_restoration_reading, loyalist_domain_factions).
narrative_ontology:constraint_beneficiary(imperial_mandate__loyalist_restoration_reading, restoration_court_nobility).
narrative_ontology:constraint_beneficiary(imperial_mandate__loyalist_restoration_reading, sonno_joi_ideologues).
narrative_ontology:constraint_victim(imperial_mandate__loyalist_restoration_reading, shogunal_bureaucracy).
narrative_ontology:constraint_victim(imperial_mandate__loyalist_restoration_reading, hereditary_samurai_retainer_class).
narrative_ontology:constraint_victim(imperial_mandate__loyalist_restoration_reading, regional_daimyo_autonomy_holders).
narrative_ontology:constraint_vindicates(imperial_mandate__loyalist_restoration_reading, unified_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(imperial_mandate__loyalist_restoration_reading, direct_imperial_rule_legitimacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Domain factions (chiefly from Satsuma, Choshu, Tosa) mobilize military and political resources under the banner of 'restoring' the emperor to direct rule. They administer the ideological campaign, draft the interpretive claim that the shogunate is illegitimate usurpation, and stand to convert loyalist service into post-restoration political power. Their exit option is real: alliance with rival factions or negotiated settlement remained open until the decision to force rupture.
narrative_ontology:constraint_stakeholder(imperial_mandate__loyalist_restoration_reading, loyalist_domain_factions, agenda_setter,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(imperial_mandate__loyalist_restoration_reading, loyalist_domain_factions, beneficiary).

% Court nobles long excluded from administrative power by centuries of shogunal precedence collect renewed ceremonial and political relevance under this reading. They benefit from the doctrine without commanding the military force that enforces it, and depend on loyalist domains to translate the doctrine into actual restoration of court authority.
narrative_ontology:constraint_stakeholder(imperial_mandate__loyalist_restoration_reading, restoration_court_nobility, beneficiary,
    moderate, generational, constrained, national).

% Scholars and activists (Mito school and successors) construct and propagate the textual and ritual argument that unmediated imperial rule is the only legitimate form of Japanese sovereignty. Their intellectual and often physical careers are fused to the doctrine's success; abandoning the reading would dissolve their standing entirely.
narrative_ontology:constraint_stakeholder(imperial_mandate__loyalist_restoration_reading, sonno_joi_ideologues, beneficiary,
    organized, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(imperial_mandate__loyalist_restoration_reading, sonno_joi_ideologues, agenda_setter).

% The Tokugawa administrative apparatus governs day-to-day affairs, treaties, and taxation under a centuries-old delegation logic. Under the loyalist reading, its entire function is recast as usurpation regardless of administrative competence. It has no coherent exit — dissolution or capitulation are its only structural options once the doctrine gains ascendancy.
narrative_ontology:constraint_stakeholder(imperial_mandate__loyalist_restoration_reading, shogunal_bureaucracy, payer,
    institutional, biographical, trapped, national).

% Retainers whose stipends, status, and identity are bound to the shogunal-daimyo hierarchy face status collapse as the doctrine delegitimizes the intermediary structure they serve. Many are absorbed into the new order at reduced standing; others lose position entirely in the Meiji administrative consolidation that follows.
narrative_ontology:constraint_stakeholder(imperial_mandate__loyalist_restoration_reading, hereditary_samurai_retainer_class, payer,
    moderate, biographical, constrained, regional).

% Domain lords who benefited from the decentralized bakuhan balance of power lose autonomous administrative standing as the doctrine demands centralization under direct imperial (in practice, new central government) authority. Their exit options narrow to negotiated absorption into the new state apparatus.
narrative_ontology:constraint_stakeholder(imperial_mandate__loyalist_restoration_reading, regional_daimyo_autonomy_holders, payer,
    powerful, generational, constrained, regional).

% Western powers negotiating treaties with the shogunate have no formal voice in the internal legitimacy contest but have enormous stake in which authority structure they must deal with going forward. Their pressure for treaty revision and modernization is cited by loyalists as evidence the old delegated structure has failed, though the treaty powers themselves are not consulted on the sovereignty question.
narrative_ontology:constraint_stakeholder(imperial_mandate__loyalist_restoration_reading, foreign_treaty_powers, excluded,
    powerful, biographical, mobile, global).

% Later scholars assess whether the Meiji Restoration constituted a genuine recovery of pre-existing sovereign authority or a novel constitutional invention retrospectively legitimated through restoration rhetoric. They have no stake in the outcome but shape how the doctrine is remembered and taught.
narrative_ontology:constraint_stakeholder(imperial_mandate__loyalist_restoration_reading, constitutional_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(imperial_mandate__loyalist_restoration_reading, loyalist_domain_factions).
narrative_ontology:fixing_cost_class(imperial_mandate__loyalist_restoration_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a unifying ideological banner under which fragmented anti-shogunate domains coordinate military and political action against a common target, converting scattered regional grievances into a single legitimacy claim capable of commanding broad loyalty.
% TRANSFER_FUNCTION: Moves administrative authority, tax base, and military command from the shogunal-daimyo hierarchy to a reconstituted imperial center (and the domain factions who staff it), while moving status and livelihood away from the hereditary retainer class and shogunal officialdom.
% ABSENT_VOICES: The shogunal administration itself, once defeated, has no forum to contest the retroactive characterization of two and a half centuries of governance as illegitimate usurpation. Ordinary subjects governed adequately under bakufu administration are not consulted on whether 'restoration' improves their situation; their preferences are absent from the doctrinal contest entirely.
% DISAPPEARANCE_RATIONALE: Without the unmediated-sovereignty doctrine, loyalist factions lose their unifying legitimacy claim and the shogunate's delegated-authority framework persists or is reformed incrementally rather than replaced by rupture; the entire Meiji institutional settlement, including conscription, prefectural abolition of domains, and the modern imperial constitutional order, depends on this doctrine having won the argument.
% FOUNDING_PROBLEM: The doctrine was constructed to resolve a genuine crisis of governmental capacity and legitimacy: the shogunate's visible inability to manage foreign treaty pressure and internal fiscal strain, which loyalists framed as proof that delegated rule itself, not merely this administration, had failed.
% FOUNDING_PROBLEM_CORROBORATION: Loyalist court chroniclers and Meiji-era state historiography attest the founding problem was real and was solved by restoration. Independent economic historians studying bakumatsu-era administrative performance, and comparative constitutional scholars outside Japan, note that the shogunate's difficulties were treaty-era external shocks compounded by fiscal strain rather than an inherent defect of delegated sovereignty as such — suggesting the 'unmediated sovereignty required' claim was doctrinally necessary for the loyalist coalition rather than empirically demonstrated by the crisis itself.
narrative_ontology:disappearance_verdict(imperial_mandate__loyalist_restoration_reading, world_rearranges).
narrative_ontology:founding_problem_status(imperial_mandate__loyalist_restoration_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(imperial_mandate__loyalist_restoration_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
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
 *   Extractiveness rises sharply through the Boshin War period (0.22 to 0.63 between 1853 and 1868) as the doctrine moves from persuasive rhetoric to the operative justification for expropriating shogunal and daimyo administrative authority, then settles somewhat (0.58-0.60) once the Meiji settlement stabilizes and extraction becomes routinized constitutional structure rather than active conquest. Suppression climbs even more steeply (0.20 to 0.75) reflecting the shift from argument to military coercion (Boshin War, subsequent suppression of samurai rebellions including Satsuma 1877) required to make the unmediated-sovereignty claim operative against a real, functioning alternative administrative structure. Theater ratio rises across the whole interval (0.15 to 0.42) as the doctrine's ceremonial and ritual apparatus — imperial processions, state Shinto construction, court ritual revival — increasingly substitutes for and legitimates administrative decisions actually made by the new oligarchic bureaucracy, not by unmediated imperial will itself. This is the central irony the reading structurally contains: the doctrine that demands the emperor personally exercise sovereignty produces, in practice, a new class of intermediaries (the Meiji oligarchs) functionally indistinguishable in role from the shogunal bureaucracy it replaced.
 *
 * DIRECTIONALITY LOGIC:
 *   Loyalist domain factions and sonno_joi ideologues sit near the beneficiary end: they set the doctrinal agenda, convert loyalist service into political capital, and collect the restructured authority. Restoration court nobility benefit without administering — their d sits moderate-low, dependent on factions to translate doctrine into power. Shogunal bureaucracy and hereditary samurai sit at the target end: trapped or constrained exit, entire governing legitimacy retroactively voided by a doctrine they had no voice in adjudicating. Foreign treaty powers are excluded from the legitimacy contest entirely despite being materially implicated in its causation — their mobility and global scope place them outside the direct extraction relationship even as their pressure is cited as justification for it.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — shogunal incapacity to manage the treaty crisis — was real but time-bound and administrative; the doctrine's status_contested marking and mismatch with disappearance_verdict=world_rearranges flags exactly the pattern the R5 interview is built to catch: a genealogy narrative (crisis proves delegated rule inherently illegitimate) constructed to justify a rupture whose actual beneficiaries were a specific loyalist coalition, not merely the immediate crisis's resolution. The corroboration record documents that independent economic historians locate the crisis in external shock and fiscal strain, not in the delegation structure itself — supporting a reading where the unmediated-sovereignty claim functioned as doctrinally necessary cover for a power transfer rather than as the crisis's actual solution. Classifying this as tangled_rope rather than pure snare preserves the genuine coordination function (unifying fragmented anti-shogunate resistance under one banner, which did solve a real collective-action problem for the loyalist coalition) while still naming the asymmetric extraction visited on the shogunal and samurai classes who had no voice in the doctrine's adjudication.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_separability_of_legitimacy_and_governance,
    'Is the emperor''s legitimacy-granting function genuinely separable from the exercise of active governance, or does the mandate require unmediated administrative sovereignty as this reading claims?',
    'This is the core structural disagreement between this reading and the bakufu_delegation_reading sibling constraint — it is not resolvable by evidence internal to either reading, since each reading''s own premises determine the answer. Resolution would require an independent theory of sacred sovereignty not itself produced by either contesting faction''s interest in the outcome.',
    'If separable (delegation reading correct), this reading''s entire delegitimization of shogunal governance is doctrinally manufactured rather than a genuine discovery of pre-existing illegitimacy, and the extraction from the shogunal/samurai class is closer to pure snare than tangled_rope. If inseparable (this reading correct), the restoration functions as genuine correction of a long-standing usurpation and the loyalist coalition''s beneficiary position is earned rather than extracted.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_separability_of_legitimacy_and_governance, conceptual, 'The located kernel disagreement: whether legitimacy and governance are separable functions of the divine mandate.').

omega_variable(
    restoration_vs_invention,
    'Did the Meiji Restoration recover a pre-existing sovereign structure that the shogunate had usurped, or did it construct a novel administrative arrangement (imperial in name, oligarchic in practice) legitimated retroactively through restoration rhetoric?',
    'Comparative institutional analysis of pre-Heian and Heian-era imperial administrative practice versus the actual operational structure of the Meiji state (genro oligarchy, cabinet system, Privy Council) would show whether ''restoration'' names genuine institutional continuity or a rhetorical frame draped over a new state form.',
    'If invention, the doctrine''s rising theater_ratio (0.15 to 0.42) is not incidental drift but the central mechanism — ritual imperial sovereignty substituting for oligarchic administrative reality exactly as the shogunate''s ritual-administrative split once did, undermining the reading''s own foundational claim against intermediary governance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(restoration_vs_invention, empirical, 'Whether the restoration was genuine institutional recovery or retrospectively legitimated invention.').

omega_variable(
    foreign_pressure_causal_weight,
    'How much of the shogunate''s perceived legitimacy failure was caused by the treaty crisis specifically, versus being a pre-existing structural vulnerability the crisis merely exposed?',
    'Comparative analysis of shogunal fiscal and administrative performance before and after Perry''s arrival (1853), controlling for treaty-related fiscal strain, would isolate the crisis''s independent causal contribution from prior structural conditions.',
    'If the crisis was the dominant cause, the founding_problem''s status as ''dead'' (since the treaty crisis was eventually resolved by any competent government) undermines the loyalist reading''s claim that delegation itself, not merely this administration, was the defect requiring institutional rupture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(foreign_pressure_causal_weight, empirical, 'Whether treaty-era crisis reveals inherent delegation defect or is a resolvable external shock.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(imperial_mandate__loyalist_restoration_reading, 1853, 1889).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(impe_tr_t1853, imperial_mandate__loyalist_restoration_reading, theater_ratio, 1853, 0.15).
narrative_ontology:measurement(impe_tr_t1860, imperial_mandate__loyalist_restoration_reading, theater_ratio, 1860, 0.22).
narrative_ontology:measurement(impe_tr_t1866, imperial_mandate__loyalist_restoration_reading, theater_ratio, 1866, 0.31).
narrative_ontology:measurement(impe_tr_t1868, imperial_mandate__loyalist_restoration_reading, theater_ratio, 1868, 0.28).
narrative_ontology:measurement(impe_tr_t1877, imperial_mandate__loyalist_restoration_reading, theater_ratio, 1877, 0.38).
narrative_ontology:measurement(impe_tr_t1889, imperial_mandate__loyalist_restoration_reading, theater_ratio, 1889, 0.42).

% Extraction over time
narrative_ontology:measurement(impe_be_t1853, imperial_mandate__loyalist_restoration_reading, base_extractiveness, 1853, 0.22).
narrative_ontology:measurement(impe_be_t1860, imperial_mandate__loyalist_restoration_reading, base_extractiveness, 1860, 0.34).
narrative_ontology:measurement(impe_be_t1866, imperial_mandate__loyalist_restoration_reading, base_extractiveness, 1866, 0.49).
narrative_ontology:measurement(impe_be_t1868, imperial_mandate__loyalist_restoration_reading, base_extractiveness, 1868, 0.63).
narrative_ontology:measurement(impe_be_t1877, imperial_mandate__loyalist_restoration_reading, base_extractiveness, 1877, 0.6).
narrative_ontology:measurement(impe_be_t1889, imperial_mandate__loyalist_restoration_reading, base_extractiveness, 1889, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(impe_su_t1853, imperial_mandate__loyalist_restoration_reading, suppression_requirement, 1853, 0.2).
narrative_ontology:measurement(impe_su_t1860, imperial_mandate__loyalist_restoration_reading, suppression_requirement, 1860, 0.4).
narrative_ontology:measurement(impe_su_t1866, imperial_mandate__loyalist_restoration_reading, suppression_requirement, 1866, 0.58).
narrative_ontology:measurement(impe_su_t1868, imperial_mandate__loyalist_restoration_reading, suppression_requirement, 1868, 0.75).
narrative_ontology:measurement(impe_su_t1877, imperial_mandate__loyalist_restoration_reading, suppression_requirement, 1877, 0.7).
narrative_ontology:measurement(impe_su_t1889, imperial_mandate__loyalist_restoration_reading, suppression_requirement, 1889, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(imperial_mandate__loyalist_restoration_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(imperial_mandate__loyalist_restoration_reading, 0.1).
narrative_ontology:affects_constraint(imperial_mandate__loyalist_restoration_reading, imperial_mandate__bakufu_delegation_reading).
narrative_ontology:affects_constraint(imperial_mandate__loyalist_restoration_reading, meiji_constitutional_sovereignty_settlement).

% DUAL FORMULATION NOTE:
% This story and imperial_mandate__bakufu_delegation_reading are the two readings of the imperial_mandate kernel decomposed per the ε-invariance principle. They share the same kernel text (the divine mandate) and the same ritual apparatus but author structurally distinct extraction profiles: this reading (loyalist restoration) authors substantial rising extractiveness driven by military coercion and retroactive delegitimization of a functioning administrative structure, while the delegation reading authors low, stable extraction consistent with centuries of co-existence between ritual and administrative sovereignty. Do not average or reconcile these ε values — they describe different constraints sharing one contested kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
