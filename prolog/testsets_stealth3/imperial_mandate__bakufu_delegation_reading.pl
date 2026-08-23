% ============================================================================
% CONSTRAINT STORY: imperial_mandate__bakufu_delegation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_imperial_mandate__bakufu_delegation_reading, []).

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
 *   constraint_id: imperial_mandate__bakufu_delegation_reading
 *   human_readable: Bakufu Delegation of Imperial Mandate (Bifurcated Sovereignty, 1192-1868)
 *   domain: political philosophy/comparative constitutional systems/east asian history
 *
 * SUMMARY:
 *   This story instantiates one reading of the imperial_mandate kernel: the
 *   bakufu_delegation_reading, in which divine mandate operates through
 *   institutional delegation — the emperor's legitimacy-granting function is
 *   separable from the governing function, which devolves to a military house
 *   holding the shogunal patent. The standing arrangement under contest is
 *   the six-century bifurcated sovereignty of Japan, 1192-1868: Kamakura,
 *   Muromachi (including the Sengoku decay), and Tokugawa phases, in which
 *   the imperial court conveyed legitimacy through investiture while warrior
 *   administrations issued law, collected levies, and monopolized force.
 *   CONSTRAINT FAMILY NOTE: the colloquial label 'imperial mandate'
 *   decomposes into at least two structurally distinct constraints. This
 *   reading authors epsilon ~= 0.55 for the delegation arrangement by its own
 *   lights (endorsed bifurcation whose transfer burden drifted past
 *   governance cost); the sibling loyalist_restoration_reading authors
 *   epsilon ~= 0.85+ for the SAME referent (unmediated-sovereignty
 *   usurpation). Same arrangement, different reading-indexed epsilon — two
 *   files, linked via network.affects_constraints, per the epsilon-invariance
 *   principle. KEY AGENTS (by structural relationship): -
 *   shogunal_bakufu_administration: agenda-setting administrator
 *   (institutional/constrained) — runs delegated governance, captures the
 *   apex share of transfers - imperial_household: dual-positioned
 *   beneficiary/payer (institutional/identity_locked) — conveys legitimacy,
 *   bears political nullity - samurai_governing_stratum: primary beneficiary
 *   (organized/constrained) — holds stipends and office justified by the
 *   delegation - agrarian_producer_households: primary target
 *   (powerless/trapped) — bears harvest assessments and corvee -
 *   court_nobility_faction: secondary payer (moderate/identity_locked) —
 *   retains rank and stipend, excluded from governance -
 *   direct_imperial_rule_advocates: suppressed challenger (organized/trapped)
 *   — bears execution, exile, purge - confucian_bakufu_scholars: analytical
 *   observer (moderate/analytical) — articulates the arrangement's
 *   justification
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(imperial_mandate__bakufu_delegation_reading, 0.55).
domain_priors:suppression_score(imperial_mandate__bakufu_delegation_reading, 0.65).
domain_priors:theater_ratio(imperial_mandate__bakufu_delegation_reading, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(imperial_mandate__bakufu_delegation_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(imperial_mandate__bakufu_delegation_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(imperial_mandate__bakufu_delegation_reading, theater_ratio, 0.6).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(imperial_mandate__bakufu_delegation_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(imperial_mandate__bakufu_delegation_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(imperial_mandate__bakufu_delegation_reading, tangled_rope).
narrative_ontology:human_readable(imperial_mandate__bakufu_delegation_reading, "Bakufu Delegation of Imperial Mandate (Bifurcated Sovereignty, 1192-1868)").
narrative_ontology:topic_domain(imperial_mandate__bakufu_delegation_reading, "political philosophy/comparative constitutional systems/east asian history").

domain_priors:requires_active_enforcement(imperial_mandate__bakufu_delegation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(imperial_mandate__bakufu_delegation_reading, 'd752f9fd-be4b-4faf-9151-a387777ec06a').
narrative_ontology:cs_kernel_codification('d752f9fd-be4b-4faf-9151-a387777ec06a', fixed_text).
narrative_ontology:cs_authority_grounding('d752f9fd-be4b-4faf-9151-a387777ec06a', lineage).
narrative_ontology:cs_interpretation_layer_present('d752f9fd-be4b-4faf-9151-a387777ec06a').
narrative_ontology:cs_reading_relation('d752f9fd-be4b-4faf-9151-a387777ec06a', imperial_mandate__loyalist_restoration_reading, forecloses).
narrative_ontology:cs_axiom('d752f9fd-be4b-4faf-9151-a387777ec06a', foundational, legitimacy_grant_separable_from_governance).
narrative_ontology:cs_axiom_status(legitimacy_grant_separable_from_governance, holdable).
narrative_ontology:cs_axiom_grounding('d752f9fd-be4b-4faf-9151-a387777ec06a', legitimacy_grant_separable_from_governance, conventional).
narrative_ontology:cs_axiom('d752f9fd-be4b-4faf-9151-a387777ec06a', secondary, delegation_serves_realm_order).
narrative_ontology:cs_axiom_status(delegation_serves_realm_order, holdable).
narrative_ontology:cs_axiom_grounding('d752f9fd-be4b-4faf-9151-a387777ec06a', delegation_serves_realm_order, instrumental).
narrative_ontology:cs_reference_frame('d752f9fd-be4b-4faf-9151-a387777ec06a', delegable_mandate_investiture_framework).
narrative_ontology:cs_drift_state('d752f9fd-be4b-4faf-9151-a387777ec06a', meiji_restoration_abolition, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('d752f9fd-be4b-4faf-9151-a387777ec06a', '').
narrative_ontology:cs_kernel_id(imperial_mandate__bakufu_delegation_reading, imperial_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(imperial_mandate__bakufu_delegation_reading, shogunal_bakufu_administration).
narrative_ontology:constraint_beneficiary(imperial_mandate__bakufu_delegation_reading, samurai_governing_stratum).
narrative_ontology:constraint_beneficiary(imperial_mandate__bakufu_delegation_reading, imperial_household).
narrative_ontology:constraint_victim(imperial_mandate__bakufu_delegation_reading, agrarian_producer_households).
narrative_ontology:constraint_victim(imperial_mandate__bakufu_delegation_reading, court_nobility_faction).
narrative_ontology:constraint_victim(imperial_mandate__bakufu_delegation_reading, direct_imperial_rule_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(imperial_mandate__bakufu_delegation_reading, court_nobility_faction).
narrative_ontology:constraint_victim(imperial_mandate__bakufu_delegation_reading, imperial_household).
narrative_ontology:constraint_vindicates(imperial_mandate__bakufu_delegation_reading, mandate_delegation_doctrine).
narrative_ontology:constraint_vindicates(imperial_mandate__bakufu_delegation_reading, ritual_executive_functional_division).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds the shogunal patent renewed by imperial investiture and governs through councils (mandokoro and hyojoshu under Kamakura; shugo networks under Muromachi; roju and metsuke under Tokugawa). Issues warrior law, apportions land rights and stipends, manages the court's access and income, and collects the largest single share of the realm's surplus (Tokugawa house lands alone approached a quarter of national output). Its authority is derivative — every regime needed the court's written sanction, and surrendering it in 1867 meant surrendering the frame of rule. Renouncing delegation entirely would mean either seizing the throne, breaking every precedent the arrangement rests on, or dissolving into ordinary warlord competition.
narrative_ontology:constraint_stakeholder(imperial_mandate__bakufu_delegation_reading, shogunal_bakufu_administration, agenda_setter,
    institutional, generational, constrained, national).

% Performs the rites from which the mandate's sanctity flows and issues the written instruments (senji, inzen, patents of appointment) that license each warrior regime. Receives stipends, court income, and unmatched ceremonial precedence; the dynasty's unbroken line is the arrangement's centerpiece and is protected accordingly. Pays for this with political nullity: no territory administered, no forces commanded, movements and marriages managed by the warrior government. Attempts to reclaim governance — the Jokyu rising of 1221, Go-Daigo's Kenmu regime of 1333-1336 — ended in military defeat, confiscation, and tightened supervision. Abdication changes the occupant, never the office; the institution cannot step outside its own sacrality.
narrative_ontology:constraint_stakeholder(imperial_mandate__bakufu_delegation_reading, imperial_household, beneficiary,
    institutional, civilizational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(imperial_mandate__bakufu_delegation_reading, imperial_household, payer).

% Staffs the administration and garrisons, holds hereditary stipends and land rights framed as reward for enforcing the delegated order. Its status, literacy, and legal privileges exist because the bifurcation assigns governing to warriors; when the arrangement wobbled (Sengoku, 1860s), the stratum faced demotion, demobilization, or ruin. Individual members could not abandon rank without losing livelihood and standing.
narrative_ontology:constraint_stakeholder(imperial_mandate__bakufu_delegation_reading, samurai_governing_stratum, beneficiary,
    organized, biographical, constrained, national).

% Farm registered land under harvest assessments (kokudaka under Tokugawa; shoto and jito levies earlier), deliver rice tax to warrior authorities, and supply corvee labor, fortification work, and the transport burdens of alternate attendance. Village headmen apportion the levy internally, but no channel carries grievances upward into the terms of settlement. Flight and land abandonment are crimes; registration, mutual-responsibility groups (goningumi), and kinship tie households to place.
narrative_ontology:constraint_stakeholder(imperial_mandate__bakufu_delegation_reading, agrarian_producer_households, payer,
    powerless, immediate, trapped, regional).

% Retains hereditary court rank, ceremonial income, and cultural primacy — the arrangement preserves the kuge as a status estate. What it does not retain is office: provincial governorships, police powers, and military command all moved to warrior hands. Households that pressed for governing roles were broken after Jokyu and again after Kenmu. Rank is hereditary identity; leaving it means ceasing to be kuge, so the estate endures its exclusion rather than exit it.
narrative_ontology:constraint_stakeholder(imperial_mandate__bakufu_delegation_reading, court_nobility_faction, payer,
    moderate, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(imperial_mandate__bakufu_delegation_reading, court_nobility_faction, beneficiary).

% Monks, court activists, and later Mito-school and sonno scholars who argue legitimacy requires the emperor's active personal rule. They hold no seat in any settlement: Jokyu leaders were executed and their estates confiscated, Kenmu partisans were purged or driven into the Southern Court's decades-long war, and Bakumatsu loyalists were imprisoned or killed. Their position is treated as rebellion rather than opposition, and each suppression cycle thins their ranks.
narrative_ontology:constraint_stakeholder(imperial_mandate__bakufu_delegation_reading, direct_imperial_rule_advocates, payer,
    organized, generational, trapped, national).
narrative_ontology:stakeholder_secondary_role(imperial_mandate__bakufu_delegation_reading, direct_imperial_rule_advocates, excluded).

% Official lecturers (the Hayashi school under the Tokugawa) and domain advisors who articulate the arrangement's justification — a hierarchic cosmos in which ritual source and executive arm each fill their appointed place — and advise on policy, schooling, and ceremony. They analyze and legitimize the structure without bearing its transfers or commanding its force.
narrative_ontology:constraint_stakeholder(imperial_mandate__bakufu_delegation_reading, confucian_bakufu_scholars, observer,
    moderate, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(imperial_mandate__bakufu_delegation_reading, shogunal_bakufu_administration).
narrative_ontology:fixing_cost_class(imperial_mandate__bakufu_delegation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: After the ritsuryo fiscal-military order decayed, the court could neither collect taxes nor suppress violence, and the victorious warrior coalition of the Genpei War held force without sacral title. The delegation arrangement joined the two: imperial patents gave warrior order-keeping legal-moral cover; warrior force protected the court and guaranteed its income; and because ruling no longer required occupying the throne, the imperial house became a prize no contender needed to destroy — dynastic continuity was the coordination good nobody else could produce.
% TRANSFER_FUNCTION: Moves agrarian surplus — harvest assessments, later commercial and transport levies — from producer households to the warrior stratum and the shogunal treasury; moves legal-moral authority from the imperial institution to whichever military house holds the patent; moves territorial administration and command from the court aristocracy to bakufu councils and domain lords.
% ABSENT_VOICES: Advocates of unmediated imperial rule were never seated: Jokyu loyalists were executed and their estates confiscated, Kenmu partisans were purged, and sonno scholars of the 1850s were imprisoned — their position was classified as rebellion, not policy. Producer households likewise had no seat; village headmen transmitted levies downward, but no mechanism carried grievances upward into the settlement's terms.
% DISAPPEARANCE_RATIONALE: Overnight disappearance forces immediate rearrangement: the warrior stratum loses the legal-moral cover that distinguishes its taxation from brigandage, the court regains a sovereignty it has no army to exercise, and every domain's title chain (traced through bakufu patents) becomes contestable. The historical analogue is 1867-1868, when surrender of the patents triggered the Boshin War and a decade of state reconstruction — conscription, prefectures, abolition of the domains — to replace what the arrangement had coordinated.
% FOUNDING_PROBLEM: Reconcile de facto warrior supremacy with a sacral monarchy that lacked coercive force: after the Genpei War, a victorious warrior coalition held real power while the only available source of supreme legitimacy was a throne it could neither occupy nor abolish.
% FOUNDING_PROBLEM_CORROBORATION: Attested from outside the benefiting parties: (a) foreign observers from the Perry expedition onward reported the split between nominal imperial sovereignty and actual shogunal administration and predicted the fiction was unstable; (b) Mito-school scholars — opponents of the arrangement — documented that the original reconciliation problem had been superseded by a warrior class transformed into salaried administrators facing foreign-pressure problems the delegation frame could not address; (c) post-Restoration Meiji leadership abolished the arrangement on the explicit ground that its mediating function was obsolete once a centralized state could fuse ritual and executive authority. No bakufu-side source independently attests the problem's death — the attestation comes entirely from critics and successors, which is itself signal.
narrative_ontology:disappearance_verdict(imperial_mandate__bakufu_delegation_reading, world_rearranges).
narrative_ontology:founding_problem_status(imperial_mandate__bakufu_delegation_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(imperial_mandate__bakufu_delegation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(imperial_mandate__bakufu_delegation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(imperial_mandate__bakufu_delegation_reading, 0.55, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(imperial_mandate__bakufu_delegation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(imperial_mandate__bakufu_delegation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(imperial_mandate__bakufu_delegation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Claim and metrics are authored independently. The claimed type, tangled_rope, states the structure I take to be true: a genuine coordination function (joining legitimacy to coercive capacity, preserving dynastic continuity, delivering the Tokugawa peace) fused with asymmetric transfer (a warrior stratum living off agrarian surplus) sustained by active enforcement against the court's political agency. The metrics describe operation as this reading assesses it. Extractiveness 0.55 is reading-indexed: this reading endorses bifurcation as such, which holds epsilon well below what the loyalist sibling authors for the same referent (~0.85), while acknowledging that the transfer burden drifted past governance cost — jito penetration of estates, hanzei half-shares, kokudaka assessments, and the sink of alternate-attendance expenditure, culminating in Tempo-era famine amid continued ceremonial outlay. Suppression 0.65 is a raw structural property, unscaled by power or scope: the arrangement's persistence required defeating the court in the field (Jokyu, Genko), extinguishing the rival line (1392), and managing the court through supervision offices and marriage politics. Theater_ratio 0.60 at interval end reflects the late-Edo phase, in which ceremonial maintenance outran adaptive function; the series shows the Sengoku spike (0.55) when the Muromachi bakufu persisted largely titularly, the Tokugawa restoration of real function (0.20), and the terminal rise. Accessibility_collapse 0.45: alternatives were never rendered unthinkable — direct rule was attempted three times in six centuries — they were defeated by force, which is why collapse stays moderate. Resistance 0.60: recurring armed challenge across the whole arc. The measurement series run on one shared nine-point grid; suppression_requirement is authored because enforcement-capacity change is the traced dynamic — build (Jokyu ratchet), decay (Onin collapse), rebuild (Tokugawa institutionalization), terminal failure (Boshin).
 *
 * PERSPECTIVAL GAP:
 *   Seats compute differently. From the shogunal administration's position the arrangement is the mandate working as designed — order delivered, functions divided, each institution doing what it alone can do. From the agrarian seat the same structure is a levy machine with no upward channel. From the court-noble seat it is gilded exclusion: rank preserved, office removed. From the imperial seat it is honored nullity — the most protected institution in the realm and the least able to act. The engine computes these divergences from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (shogunal administration, samurai stratum, imperial household) drive low directionality for those seats; victim declarations (agrarian households, court faction, direct-rule advocates) drive high directionality. Trapped exits push the agrarian and advocate seats toward the full-target end; identity_locked exit pulls the imperial household and court faction toward the middle despite their declared roles — the household because its beneficiary side (status, income, protection) is real and its payer side (political nullity) is borne by an institution that cannot leave, the faction because retained rank and stipend temper its exclusion. No directionality_overrides are authored: the dual positions are encoded through secondary_role declarations, and the override mechanism keys on power_atom alone, which could not separate the two institutional/moderate seats without colliding.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against two opposite errors. Calling the arrangement a rope would erase the enforced asymmetry — the coordination was real, but it rode on suppressing the court's political agency and on transfers with no upward accountability. Calling it a snare would erase the founding bargain, which solved a problem no contemporary alternative solved and delivered the longest peace in Japanese history. Tangled_rope holds both. The late-interval record shows piton symptoms — theater_ratio 0.60, founding problem dead, persistence by inertia and vested interest — and the R5 mismatch (status dead x verdict world_rearranges) flags the zombie phase for the consumer without retroactively reclassifying the centuries in which the structure performed. Mandatrophy resolution: the mediating function atrophied as the warrior class turned into a salaried administration; the arrangement persisted on ceremonial momentum until external shock (Perry) exposed the gap and the loyalist sibling reading won the terminal contest.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_index_epsilon_divergence,
    'The loyalist_restoration_reading authors epsilon for this same delegation arrangement near 0.85 (wholesale usurpation of imperial sovereignty); this reading authors 0.55 (endorsed bifurcation with acknowledged rent drift). Which value governs classification?',
    'None resolves it — epsilon is reading-indexed by design (OQ-26); the corpus retains both files and compares classifications across readings as the measurement itself.',
    'Per-seat and per-reading classifications diverge sharply over the identical referent; cross-reading comparison, not adjudication, is the output.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_index_epsilon_divergence, conceptual, 'Same arrangement, two reading-indexed epsilon values; divergence is structural, not error.').

omega_variable(
    foreclosure_vs_recurred_attempts,
    'This reading conceptually forecloses unmediated imperial rule (within its frame the loyalist demand is a category error), yet direct-rule attempts recurred for six centuries (Jokyu 1221, Kenmu 1333-1336, sonno joi 1850s-60s). Is the foreclosure edge structurally real or merely aspirational?',
    'Survey bakufu-framework doctrinal texts for any accommodation of unmediated imperial governance without contradiction; test whether loyalist positions were ever held inside a bakufu-consistent framework rather than outside it.',
    'If foreclosure is soft, the reading_relations edge downgrades to influences and the sibling remains co-holdable in hybrid frameworks (e.g., constitutional symbolism with separated ritual and executive); if hard, the engine computes the sibling as foreclosed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(foreclosure_vs_recurred_attempts, conceptual, 'Whether the axiom-level contradiction produces genuine structural displacement of the sibling reading.').

omega_variable(
    founding_problem_death_dating,
    'When did the founding problem (raw warrior supremacy needing sacral cover) die relative to the arrangement''s persistence to 1868?',
    'Date the transformation of the warrior class into salaried administration (domain finance records, 17th-18th century) and the shift of crisis to foreign relations; corroborate with contemporaneous reform cycles (Kyoho, Kansei, Tempo) addressing problems the founding frame cannot name.',
    'Supports founding_problem_status=dead and fires the dead-x-world_rearranges mismatch flag; refines mandatrophy dating to roughly the Tempo era.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_death_dating, empirical, 'Dating the death of the founding problem against the arrangement''s terminal persistence.').

omega_variable(
    investiture_efficacy,
    'Did imperial investiture causally confer legitimacy on warrior regimes, or was it retrospective ratification purchased after force had already decided outcomes?',
    'Compare regime trajectories with prompt versus delayed or contested investiture (Ashikaga Takauji''s interregnum, Northern and Southern parallel investitures, Tokugawa court management); test whether legitimacy shortfalls predict instability independent of military position.',
    'If sanction is ex-post formality, the emperor''s legitimacy-granting function is closer to performance than substance, raising theater_ratio and weakening the coordination-function claim; if causal, the bifurcation is a genuine production process for legitimacy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(investiture_efficacy, empirical, 'Causal weight of the imperial grant in sustaining warrior regimes.').

omega_variable(
    suppression_structural_vs_doctrinal,
    'Is the measured suppression of imperial political agency structural (military defeat, confiscation, court supervision) or doctrinal (court elites internalizing bifurcation as the natural order)?',
    'Post-abolition trajectory: after 1868 the court immediately claimed and exercised direct sovereignty (osei fukko, then active rule) — rapid reversion indicates the prior quiescence was held down structurally rather than internalized.',
    'If a large internalized share existed, suppression would persist in form after enforcement removal; the observed immediate reversion supports a predominantly structural reading for the pre-1868 period.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_doctrinal, empirical, 'Composition of the suppression that held imperial political agency at zero.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(imperial_mandate__bakufu_delegation_reading, 1192, 1868).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(impe_tr_t1192, imperial_mandate__bakufu_delegation_reading, theater_ratio, 1192, 0.15).
narrative_ontology:measurement_basis(impe_tr_t1192, observed).
narrative_ontology:measurement(impe_tr_t1221, imperial_mandate__bakufu_delegation_reading, theater_ratio, 1221, 0.18).
narrative_ontology:measurement_basis(impe_tr_t1221, observed).
narrative_ontology:measurement(impe_tr_t1336, imperial_mandate__bakufu_delegation_reading, theater_ratio, 1336, 0.22).
narrative_ontology:measurement_basis(impe_tr_t1336, observed).
narrative_ontology:measurement(impe_tr_t1392, imperial_mandate__bakufu_delegation_reading, theater_ratio, 1392, 0.25).
narrative_ontology:measurement_basis(impe_tr_t1392, observed).
narrative_ontology:measurement(impe_tr_t1467, imperial_mandate__bakufu_delegation_reading, theater_ratio, 1467, 0.55).
narrative_ontology:measurement_basis(impe_tr_t1467, observed).
narrative_ontology:measurement(impe_tr_t1600, imperial_mandate__bakufu_delegation_reading, theater_ratio, 1600, 0.2).
narrative_ontology:measurement_basis(impe_tr_t1600, observed).
narrative_ontology:measurement(impe_tr_t1716, imperial_mandate__bakufu_delegation_reading, theater_ratio, 1716, 0.28).
narrative_ontology:measurement_basis(impe_tr_t1716, observed).
narrative_ontology:measurement(impe_tr_t1853, imperial_mandate__bakufu_delegation_reading, theater_ratio, 1853, 0.45).
narrative_ontology:measurement_basis(impe_tr_t1853, observed).
narrative_ontology:measurement(impe_tr_t1868, imperial_mandate__bakufu_delegation_reading, theater_ratio, 1868, 0.6).
narrative_ontology:measurement_basis(impe_tr_t1868, observed).

% Extraction over time
narrative_ontology:measurement(impe_be_t1192, imperial_mandate__bakufu_delegation_reading, base_extractiveness, 1192, 0.26).
narrative_ontology:measurement_basis(impe_be_t1192, observed).
narrative_ontology:measurement(impe_be_t1221, imperial_mandate__bakufu_delegation_reading, base_extractiveness, 1221, 0.3).
narrative_ontology:measurement_basis(impe_be_t1221, observed).
narrative_ontology:measurement(impe_be_t1336, imperial_mandate__bakufu_delegation_reading, base_extractiveness, 1336, 0.36).
narrative_ontology:measurement_basis(impe_be_t1336, observed).
narrative_ontology:measurement(impe_be_t1392, imperial_mandate__bakufu_delegation_reading, base_extractiveness, 1392, 0.37).
narrative_ontology:measurement_basis(impe_be_t1392, observed).
narrative_ontology:measurement(impe_be_t1467, imperial_mandate__bakufu_delegation_reading, base_extractiveness, 1467, 0.4).
narrative_ontology:measurement_basis(impe_be_t1467, observed).
narrative_ontology:measurement(impe_be_t1600, imperial_mandate__bakufu_delegation_reading, base_extractiveness, 1600, 0.44).
narrative_ontology:measurement_basis(impe_be_t1600, observed).
narrative_ontology:measurement(impe_be_t1716, imperial_mandate__bakufu_delegation_reading, base_extractiveness, 1716, 0.46).
narrative_ontology:measurement_basis(impe_be_t1716, observed).
narrative_ontology:measurement(impe_be_t1853, imperial_mandate__bakufu_delegation_reading, base_extractiveness, 1853, 0.52).
narrative_ontology:measurement_basis(impe_be_t1853, observed).
narrative_ontology:measurement(impe_be_t1868, imperial_mandate__bakufu_delegation_reading, base_extractiveness, 1868, 0.55).
narrative_ontology:measurement_basis(impe_be_t1868, observed).

% Suppression requirement over time
narrative_ontology:measurement(impe_su_t1192, imperial_mandate__bakufu_delegation_reading, suppression_requirement, 1192, 0.4).
narrative_ontology:measurement_basis(impe_su_t1192, observed).
narrative_ontology:measurement(impe_su_t1221, imperial_mandate__bakufu_delegation_reading, suppression_requirement, 1221, 0.55).
narrative_ontology:measurement_basis(impe_su_t1221, observed).
narrative_ontology:measurement(impe_su_t1336, imperial_mandate__bakufu_delegation_reading, suppression_requirement, 1336, 0.7).
narrative_ontology:measurement_basis(impe_su_t1336, observed).
narrative_ontology:measurement(impe_su_t1392, imperial_mandate__bakufu_delegation_reading, suppression_requirement, 1392, 0.6).
narrative_ontology:measurement_basis(impe_su_t1392, observed).
narrative_ontology:measurement(impe_su_t1467, imperial_mandate__bakufu_delegation_reading, suppression_requirement, 1467, 0.45).
narrative_ontology:measurement_basis(impe_su_t1467, observed).
narrative_ontology:measurement(impe_su_t1600, imperial_mandate__bakufu_delegation_reading, suppression_requirement, 1600, 0.75).
narrative_ontology:measurement_basis(impe_su_t1600, observed).
narrative_ontology:measurement(impe_su_t1716, imperial_mandate__bakufu_delegation_reading, suppression_requirement, 1716, 0.65).
narrative_ontology:measurement_basis(impe_su_t1716, observed).
narrative_ontology:measurement(impe_su_t1853, imperial_mandate__bakufu_delegation_reading, suppression_requirement, 1853, 0.6).
narrative_ontology:measurement_basis(impe_su_t1853, observed).
narrative_ontology:measurement(impe_su_t1868, imperial_mandate__bakufu_delegation_reading, suppression_requirement, 1868, 0.5).
narrative_ontology:measurement_basis(impe_su_t1868, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(imperial_mandate__bakufu_delegation_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(imperial_mandate__bakufu_delegation_reading, loyalist_restoration_reading).

% DUAL FORMULATION NOTE:
% Decomposition of the 'imperial mandate' label per the epsilon-invariance principle: the delegation reading and the loyalist reading assess the SAME standing arrangement (bifurcated sovereignty, 1192-1868) with different reading-indexed epsilon (~0.55 vs ~0.85+) and different victim sets (the loyalist reading adds the emperor's sovereignty itself to the victim roll). Logical relation: foreclosure — the axioms are contradictories. Structural relation: this reading was the operative constraint for six centuries, and its suppressions (Jokyu, Kenmu, Bakumatsu persecutions) created the grievance reservoir that financed the sibling's eventual victory; the network edge records that downstream pressure on the sibling's operating environment.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
