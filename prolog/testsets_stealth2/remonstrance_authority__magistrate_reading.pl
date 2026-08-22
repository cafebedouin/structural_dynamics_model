% ============================================================================
% CONSTRAINT STORY: remonstrance_authority__magistrate_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_remonstrance_authority__magistrate_reading, []).

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
 *   constraint_id: remonstrance_authority__magistrate_reading
 *   human_readable: Remonstrance Authority of the French Sovereign Courts (Magistrate Reading)
 *   domain: constitutional_history/political_economy/legal_authority
 *
 * SUMMARY:
 *   In eighteenth-century France, royal edicts acquired binding force only
 *   after registration by the sovereign courts (parlements), which could
 *   delay and refuse them through written remonstrances invoking the
 *   kingdom's 'fundamental laws.' The magistrate reading — the parlementaire
 *   self-understanding, elaborated doctrinally in the 1750s–60s — holds this
 *   gate to be a fundamental constitutional mechanism preserving ancient
 *   liberties against arbitrary royal innovation. This file instantiates THAT
 *   reading as a clean, ε-invariant constraint: the referent of ε is the
 *   standing registration/remonstrance arrangement itself, assessed by the
 *   reading's own lights — never the rival reading's characterization of it.
 *   Even on the reading's own terms, the arrangement's operation on fiscal
 *   reform edicts is substantially costly to those it governs: blocked reform
 *   preserved privileged exemptions and held incidence on commoner taxpayers,
 *   while sustaining the magistracy's corporate power, fees, and tax-exempt
 *   office property. The crown could override the gate (lit de justice,
 *   exile, the Maupeou suppression of 1771) only at mounting legitimacy cost,
 *   making enforcement escalation the arrangement's central dynamic. FAMILY
 *   NOTE: the colloquial label 'the remonstrance right' covers two
 *   structurally distinct claims — this magistrate reading and the sibling
 *   crown_reading (remonstrance_authority__crown_reading), which instantiates
 *   the same referent arrangement as an illegitimate minoritarian veto. The
 *   readings share one referent and diverge on legitimacy attribution and
 *   victim identification; ε here is reading-indexed to the magistrate seat,
 *   and the sibling authors its own ε over the identical arrangement.
 *   Claim/metric independence: the reading CLAIMS a fundamental
 *   constitutional mechanism; the authored metrics describe costly, actively
 *   enforced, increasingly theatrical operation — that divergence is the
 *   datum, not an error to reconcile.
 *
 * KEY AGENTS:
 *   - parlement_magistracy: Agenda-setting beneficiary (institutional / identity_locked) — runs the registration gate, collects fees and preserved exemptions; conditional victim under crown override
 *   - french_crown: Checked agenda-setter (institutional / constrained) — drafts edicts, compels registration, bears fiscal rigidity and legitimacy costs
 *   - commoner_taxpayers: Primary target (powerless / trapped) — bear the tax incidence the gate preserves; no seat in the process
 *   - privileged_orders: Secondary beneficiary (powerful / identity_locked) — collect preserved exemptions without operating the gate
 *   - reform_ministers: Payer (moderate / constrained) — their edicts die in remonstrance cycles; careers end with them
 *   - provincial_parlements: Coordinating agenda-setter tier (organized / identity_locked) — local gates and class unions; episodic exile victims
 *   - estates_general_forum: Excluded alternative (organized / trapped) — the unconvoked consent mechanism whose absence keeps the contest inside the courts
 *   - physiocrat_analysts: Analytical observer (moderate / analytical) — diagnose the gate as privilege defense from outside; anticipate the crown reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(remonstrance_authority__magistrate_reading, 0.78).
domain_priors:suppression_score(remonstrance_authority__magistrate_reading, 0.84).
domain_priors:theater_ratio(remonstrance_authority__magistrate_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(remonstrance_authority__magistrate_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(remonstrance_authority__magistrate_reading, suppression_requirement, 0.84).
narrative_ontology:constraint_metric(remonstrance_authority__magistrate_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(remonstrance_authority__magistrate_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(remonstrance_authority__magistrate_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(remonstrance_authority__magistrate_reading, tangled_rope).
narrative_ontology:human_readable(remonstrance_authority__magistrate_reading, "Remonstrance Authority of the French Sovereign Courts (Magistrate Reading)").
narrative_ontology:topic_domain(remonstrance_authority__magistrate_reading, "constitutional_history/political_economy/legal_authority").

domain_priors:requires_active_enforcement(remonstrance_authority__magistrate_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(remonstrance_authority__magistrate_reading, '378b00e7-270f-49ed-99fc-b76080a184fa').
narrative_ontology:cs_kernel_codification('378b00e7-270f-49ed-99fc-b76080a184fa', distributed).
narrative_ontology:cs_authority_grounding('378b00e7-270f-49ed-99fc-b76080a184fa', lineage).
narrative_ontology:cs_interpretation_layer_present('378b00e7-270f-49ed-99fc-b76080a184fa').
narrative_ontology:cs_reading_relation('378b00e7-270f-49ed-99fc-b76080a184fa', remonstrance_authority__crown_reading, forecloses).
narrative_ontology:cs_axiom('378b00e7-270f-49ed-99fc-b76080a184fa', foundational, remonstrance_right_is_part_of_fundamental_constitution).
narrative_ontology:cs_axiom_status(remonstrance_right_is_part_of_fundamental_constitution, holdable).
narrative_ontology:cs_axiom_grounding('378b00e7-270f-49ed-99fc-b76080a184fa', remonstrance_right_is_part_of_fundamental_constitution, conventional).
narrative_ontology:cs_axiom('378b00e7-270f-49ed-99fc-b76080a184fa', foundational, edicts_bind_only_after_free_registration).
narrative_ontology:cs_axiom_status(edicts_bind_only_after_free_registration, holdable).
narrative_ontology:cs_axiom_grounding('378b00e7-270f-49ed-99fc-b76080a184fa', edicts_bind_only_after_free_registration, conventional).
narrative_ontology:cs_reference_frame('378b00e7-270f-49ed-99fc-b76080a184fa', ancient_constitution_custodial_check).
narrative_ontology:cs_drift_state('378b00e7-270f-49ed-99fc-b76080a184fa', late_ancien_regime_fiscal_crisis, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('378b00e7-270f-49ed-99fc-b76080a184fa', '').
narrative_ontology:cs_kernel_id(remonstrance_authority__magistrate_reading, remonstrance_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(remonstrance_authority__magistrate_reading, parlement_magistracy).
narrative_ontology:constraint_beneficiary(remonstrance_authority__magistrate_reading, provincial_parlements).
narrative_ontology:constraint_beneficiary(remonstrance_authority__magistrate_reading, privileged_orders).
narrative_ontology:constraint_victim(remonstrance_authority__magistrate_reading, commoner_taxpayers).
narrative_ontology:constraint_victim(remonstrance_authority__magistrate_reading, parlement_magistracy).
narrative_ontology:constraint_victim(remonstrance_authority__magistrate_reading, provincial_parlements).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(remonstrance_authority__magistrate_reading, french_crown).
narrative_ontology:constraint_victim(remonstrance_authority__magistrate_reading, reform_ministers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds venal, effectively hereditary seats on the sovereign courts. Gates every royal edict: registers, delays, or refuses it with written remonstrances invoking the kingdom's fundamental laws. Collects registration fees and judicial dues, and preserves the tax exemptions attached to office and rank. Enforces its position through remonstrance cycles, class unions with the provincial courts, and published arrêts. When the crown compels registration by lit de justice, exiles a court, or (1771) abolishes the courts outright, magistrates lose salary, office value, and public standing — the override episodes are the one channel through which the arrangement costs them, and they are episodic beside four centuries of gate-keeping.
narrative_ontology:constraint_stakeholder(remonstrance_authority__magistrate_reading, parlement_magistracy, agenda_setter,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(remonstrance_authority__magistrate_reading, parlement_magistracy, payer).

% Drafts and promulgates edicts, convenes the lit de justice to compel registration, and can restructure or exile courts. Every major fiscal initiative must survive remonstrance cycles or be forced through at rising legitimacy cost. Reign-bound decision-making shortens its horizon even as dynastic debt lengthens the bill. It bears the fiscal rigidity the gate produces — borrowing premia, partial defaults, the 1788 payment stop — and absorbs political blame for outcomes the gate shaped. Its workarounds (new courts, assemblies of notables, provincial assemblies) each carried heavy setup and legitimacy costs, so it routes around the gate rarely and at price.
narrative_ontology:constraint_stakeholder(remonstrance_authority__magistrate_reading, french_crown, agenda_setter,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(remonstrance_authority__magistrate_reading, french_crown, payer).

% Bear the taille, gabelle, capitation, vingtième, militia levy, and corvée burdens that reform edicts would have broadened or lightened. They hold no seat in registration: no estate representation inside the gate, no petition standing before the sovereign courts, and remonstrances speak for 'the nation' without them. Emigration or evasion is available only at ruinous cost for most; they learn of edicts after the gate has acted.
narrative_ontology:constraint_stakeholder(remonstrance_authority__magistrate_reading, commoner_taxpayers, payer,
    powerless, biographical, trapped, national).

% Nobility and clergy hold the exemptions from taille and most direct taxes that universal-tax edicts would abolish. They do not operate the gate; they collect its output whenever a fiscal edict dies in remonstrance. Their status identity is constituted by the ordered society of ranks itself, so defending exemption and defending self are the same act, and exit would mean ceasing to be what they are.
narrative_ontology:constraint_stakeholder(remonstrance_authority__magistrate_reading, privileged_orders, beneficiary,
    powerful, generational, identity_locked, national).

% Controllers-general and chancellors — Machault, Turgot, Calonne, Brienne, Lamoignon — draft fiscal reform edicts, spend political capital shepherding them through registration, and absorb the outcome: edicts softened or killed in remonstrance, careers ended by dismissal when the crown retreats. Their exit is retirement or disgrace; they cannot relocate the gate or extend their own tenure past the king's patience.
narrative_ontology:constraint_stakeholder(remonstrance_authority__magistrate_reading, reform_ministers, payer,
    moderate, immediate, constrained, national).

% Thirteen provincial sovereign courts — Toulouse, Rennes, Bordeaux, Rouen, Aix, Besançon and the rest — run the same registration gate for their ressorts, collect comparable fees and office-linked exemptions, and coordinate with the Paris court through class unions and combined remonstrances. Individually weaker than Paris, they are the crown's preferred targets for discipline: repeated exiles (Rennes during the Brittany affair, Bordeaux in 1768) and, in 1771, wholesale abolition and replacement by restructured tribunals.
narrative_ontology:constraint_stakeholder(remonstrance_authority__magistrate_reading, provincial_parlements, agenda_setter,
    organized, generational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(remonstrance_authority__magistrate_reading, provincial_parlements, payer).

% The kingdom's traditional consultative assembly, unconvoked since 1614. Fiscal reformers pointed to it as the legitimate alternative consent mechanism for taxation; the crown avoided summoning it precisely because its writ would exceed the courts' gate, and the courts had no interest in a rival forum. Its exclusion is what kept registration politics inside the judicial corporations. When finally convoked in 1789 it displaced the arrangement entirely within months.
narrative_ontology:constraint_stakeholder(remonstrance_authority__magistrate_reading, estates_general_forum, excluded,
    organized, generational, trapped, national).

% Quesnay, Le Trosne, Dupont de Nemours and the Éphémérides circle analyze the gate from outside: they advocate that edicts bind without judicial consent ('legal despotism') and diagnose the remonstrance right as privilege defense in constitutional dress. They publish, advise ministers, and hold no procedural seat; their diagnosis anticipates the rival reading of the same institution.
narrative_ontology:constraint_stakeholder(remonstrance_authority__magistrate_reading, physiocrat_analysts, observer,
    moderate, biographical, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(remonstrance_authority__magistrate_reading, parlement_magistracy).
narrative_ontology:fixing_cost_class(remonstrance_authority__magistrate_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Gates royal legislation through judicial registration: maintains legal continuity across reigns, records a documented act of consent for each edict, subjects drafts to scrutiny and delay that feed back into drafting, and anchors uniform administration of justice across a legally plural kingdom of customs, pays d'état, and pays d'élections.
% TRANSFER_FUNCTION: Moves fiscal discretion and tax incidence: blocks the transfer of tax burden onto privileged wealth, keeping it on commoner taxpayers; moves registration fees, judicial dues, and office-value appreciation to the magistracy; and moves the cost of legislative initiative to the crown, which must buy each edict through remonstrance cycles or forced registration.
% ABSENT_VOICES: Commoner taxpayers had no seat anywhere in the registration contest — the Estates-General had not met since 1614, provincial estates were weak or absent in most of the kingdom, and petitions had no standing before the sovereign courts. Rural communities bearing the corvée and gabelle learned of edicts only after the gate had acted. Their absence meant the 'nation' invoked in remonstrances was articulated solely by the corporate bodies doing the remonstrating.
% DISAPPEARANCE_RATIONALE: If the registration gate vanished overnight, fiscal edicts would bind on promulgation: the Maupeou interlude (1771–74) is the controlled demonstration — with the old courts suppressed, Terray's partial bankruptcy and the free-grain trade edicts registered without remonstrance cycles. Office values would collapse, the magistracy's corporate power and exemptions would become legislatively vulnerable, and the entire fiscal-political settlement of privilege would rearrange around whatever consent mechanism replaced the gate.
% FOUNDING_PROBLEM: How can royal legislation bind a legally plural kingdom without appearing arbitrary — recording consent, screening edicts against customary liberties and local law, and maintaining legal continuity across reigns — given that the king legislates alone?
% FOUNDING_PROBLEM_CORROBORATION: No seat inside the beneficiary set can settle this. External attestation of the shifted function: Maupeou-era justificatory tracts and the physiocrat literature (Le Trosne, the Éphémérides circle) — both outside the parlementaire beneficiary set — attest that by the 1770s the gate screened chiefly for privilege and office property; crown council memoranda and the memoirs of Turgot and Calonne corroborate from the payer side. Attestation that the founding problem was live earlier rests on sixteenth- and seventeenth-century registration practice and the 1750s remonstrance texts, though the latter are self-interested sources. No disinterested arbiter exists; the strongest external corroboration supports a founding problem substantially transformed by the 1770s, which is why the status is contested rather than live.
narrative_ontology:disappearance_verdict(remonstrance_authority__magistrate_reading, world_rearranges).
narrative_ontology:founding_problem_status(remonstrance_authority__magistrate_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(remonstrance_authority__magistrate_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(remonstrance_authority__magistrate_reading, 'none', 1).
narrative_ontology:epsilon_provenance(remonstrance_authority__magistrate_reading, 0.78, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(remonstrance_authority__magistrate_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(remonstrance_authority__magistrate_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(remonstrance_authority__magistrate_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78 at interval end) because the gate's principal eighteenth-century operation on money bills was refusal, and the incidence of preserved fiscal inequity fell on people with no seat in the process; the expected structural delta for this reading — high cost on fiscal reform edicts, a beneficiary class of tax-exempt magistracy — is authored directly. Suppression (0.84) is authored as a RAW STRUCTURAL PROPERTY, unscaled: the engine scales only extractiveness, by directionality and scope. Persistence depended on active enforcement — the registration ceremony, remonstrance cycles, class unions — and on the crown's escalating counter-coercion, which peaked with the 1771 suppression of the courts themselves. Theater (0.55) rose across the interval: by the 1780s the constitutional idiom of ancient liberties and national trusteeship was substantially performative covering the defense of office property and exemption schedules, though the judicial screening function remained real. Accessibility collapse (0.58): alternatives existed — forced registration, new courts, assemblies of notables and provincial assemblies, the Estates-General — but each carried heavy legitimacy or financial cost, and venal office property made abolition expensive enough that even the 1771 coup left compensation liabilities. Resistance (0.7): constant crown counter-action, pamphlet wars, ministerial pressure, and finally the revolutionary dissolution of the gate's carriers. CYCLICAL DYNAMICS: the series oscillates around a coercion–capitulation–restoration cycle (buildup 1756–63, rupture 1771, restoration 1776, terminal ratchet 1783–89) — each capitulation restored the gate with added prestige, which encouraged deeper obstruction next round; the oscillation itself functions as intermittent reinforcement, not noise. Base properties are measured at interval end (terminal-crisis phase). COALITION NOTE: commoner taxpayers are individually powerless, but their latent coalition power materialized exactly once — the Estates-General of 1789 — and dissolved the arrangement within months; the gate's stability presupposed keeping that coalition unconvoked.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute different arrangements from the same structure. From the payer seats (commoner taxpayers, reform ministers), the gate operates as enforced obstruction whose costs land on the unrepresented. From the magistracy seat, the same gate is custodial coordination — the corporation experiences its own obstruction as constitutional duty, an identity fusion of office-as-property with office-as-vocation that makes exit unthinkable (identity_locked): breaking the frame, as Maupeou briefly broke it in 1771, revealed that cooperation was available all along once the identity frame was shattered and rebuilt around restructured tribunals. From the crown seat, the gate is a legitimacy tax on governance — payable in delay, ceremony, or coercion. INTER-INSTITUTIONAL DYNAMICS: crown and parlements hold nominally comparable institutional power, yet experience the constraint oppositely because their exit options differ — the crown can route around (new courts, notables) at legitimacy cost; the magistracy cannot leave its own office without ceasing to be itself. SAME-LEVEL LATERAL DYNAMICS: the Paris court and the provincial parlements hold the same right at different power levels (institutional vs organized); Paris anchors the class unions and absorbs the decisive confrontations, while provincial courts suffer the crown's preferred disciplinary instrument — exile — making their effective exposure to override systematically higher despite identical formal position.
 *
 * DIRECTIONALITY LOGIC:
 *   Declarations map to poles as follows. The magistracy (both tiers) sits at the beneficiary pole: standing collectors of fees, office value, and preserved exemption, running the gate they administer. The privileged orders sit near them: pure collectors of the gate's output. Commoner taxpayers sit at the target pole — trapped exit amplifies their effective position toward full target. Reform ministers sit target-side: they supply the edicts the gate consumes and absorb the career cost. The crown is deliberately NOT declared a victim in this reading: its constraint by the gate is the arrangement's function, not its extraction, and its workarounds keep it near-symmetric with a slight target tilt. The magistracy's dual listing (beneficiary and victim) encodes the override episodes — lit de justice, exile, the 1771 abolition — which are real but episodic against four centuries of gate-keeping; the standing relationship dominates. NO DIRECTIONALITY OVERRIDE is authored, deliberately: overrides key on the power atom, and the only institutional-power agents in this story are the magistracy and the crown, so any override correcting the magistracy's d would simultaneously corrupt the crown's. The ambiguity is routed to the omega variable magistrate_dual_position_weighting instead. Scope: the arrangement operates at national scope, which modestly amplifies effective extraction for targets (verification of edict-level incidence is harder across a kingdom of customs than inside one court's ressort).
 *
 * MANDATROPHY ANALYSIS:
 *   The magistrate reading's framing invites two misclassifications. Read as its proponents framed it — a fundamental law, older than memory, above choice — the gate codes as mountain, and the FSM signature would fire on the declared beneficiaries; read as its opponents framed it — pure veto — it codes as snare, erasing the real screening, consent-recording, and continuity functions the gate performed for centuries. Authoring the full structural data (coordination beneficiaries AND victims AND active enforcement) lets the engine compute tangled_rope: a genuine coordination function carrying asymmetric extraction through the same structure. The mandatrophy question is the R5 mismatch: the founding problem (consent-screening for royal legislation in a legally plural kingdom) progressively died as the fundamental-law vocabulary detached from any enumerable content — by the 1770s the 'ancient constitution' invoked in remonstrances was largely a doctrine elaborated within living memory — while the arrangement persisted on office property, fee income, and corporate identity. Status contested × verdict world_rearranges is exactly the capture/zombie configuration the R5 consumer cross-checks against the computed theater path.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This constraint is one reading of the kernel remonstrance_authority — the magistrate_reading. What structurally changes if the sibling reading (crown_reading) is adopted instead, and where exactly is the disagreement located?',
    'Adopting the crown_reading re-keys the same referent arrangement: the crown and the reform program enter the victim set, the magistracy moves from beneficiary to agenda-setting extractor, ε is re-authored upward over the identical referent, and the computed type shifts toward snare. The disagreement is located in the legitimacy predicate applied to the registration gate itself — not in any observable of the gate''s operation.',
    'Classification flips along the legitimacy axis while the referent arrangement stays fixed; cross-reading comparison of the two files measures how much of the type verdict is reading-indexed versus structural.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: this file is the magistrate_reading of kernel remonstrance_authority; the crown_reading is a separate constraint over the same referent.').

omega_variable(
    ancient_constitution_continuity,
    'Is the remonstrance right a genuinely continuous mechanism descending from medieval registration practice, or a juridical construction elaborated in the 1750s–60s (Le Paige''s Lettres historiques, Mey''s Tractatus) and retrojected onto antiquity?',
    'Archival comparison of sixteenth- and seventeenth-century registration practice (register contents, lit de justice frequency, actual delay patterns) against the eighteenth-century doctrinal elaboration that first systematized ''fundamental laws.''',
    'If the reference frame is a retrojection, the reading''s authority claim loses its lineage warrant and the arrangement weights toward pure extraction; if continuous, more coordination weight survives and the tangled_rope reading strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ancient_constitution_continuity, empirical, 'Whether the magistrate reading''s reference frame describes a continuous institution or an invented antiquity.').

omega_variable(
    magistrate_dual_position_weighting,
    'The magistracy appears in both the beneficiary and victim sets — standing collector of fees, office value, and preserved exemption, yet conditional victim under crown override (lit de justice, exile, the 1771 abolition). Which relationship dominates its directionality?',
    'Weight the episodes by duration and severity: four centuries of gate-keeping and fee collection against episodic override losses concentrated in 1756–71 and 1787–89. If override-victimhood dominates, the magistracy''s d rises toward the target pole and measured asymmetry falls; if standing benefit dominates, d stays near the beneficiary pole.',
    'Determines whether the arrangement computes as magistracy-subsidized (low d, amplified asymmetry elsewhere) or as mutually damaging (compressed asymmetry, weaker tangled_rope signal).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(magistrate_dual_position_weighting, empirical, 'How to weight the magistracy''s dual beneficiary/victim membership in the directionality derivation.').

omega_variable(
    epsilon_incidence_attribution,
    'How much of the measured ε belongs to the registration gate itself, versus to the wider privilege economy (venal office, guild restriction, provincial franchise) that the gate merely shielded from legislative touch?',
    'Counterfactual yield modeling of the killed edicts — Turgot''s subvention territoriale, Calonne''s universal land tax — against the fiscal outcomes of the Maupeou window when the gate was closed but the wider privilege economy persisted.',
    'If most measured ε persists with the gate closed, ε belongs to sibling constraints in the wider privilege family and this story''s ε is overstated; if it tracks the gate, the attribution stands and the family boundary drawn here is correct.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(epsilon_incidence_attribution, conceptual, 'Family-decomposition boundary: gate-attributed versus privilege-economy-attributed shares of the measured extraction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(remonstrance_authority__magistrate_reading, 1750, 1789).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(remo_tr_t1750, remonstrance_authority__magistrate_reading, theater_ratio, 1750, 0.24).
narrative_ontology:measurement(remo_tr_t1756, remonstrance_authority__magistrate_reading, theater_ratio, 1756, 0.28).
narrative_ontology:measurement(remo_tr_t1763, remonstrance_authority__magistrate_reading, theater_ratio, 1763, 0.33).
narrative_ontology:measurement(remo_tr_t1771, remonstrance_authority__magistrate_reading, theater_ratio, 1771, 0.2).
narrative_ontology:measurement(remo_tr_t1776, remonstrance_authority__magistrate_reading, theater_ratio, 1776, 0.38).
narrative_ontology:measurement(remo_tr_t1783, remonstrance_authority__magistrate_reading, theater_ratio, 1783, 0.44).
narrative_ontology:measurement(remo_tr_t1787, remonstrance_authority__magistrate_reading, theater_ratio, 1787, 0.51).
narrative_ontology:measurement(remo_tr_t1789, remonstrance_authority__magistrate_reading, theater_ratio, 1789, 0.55).

% Extraction over time
narrative_ontology:measurement(remo_be_t1750, remonstrance_authority__magistrate_reading, base_extractiveness, 1750, 0.56).
narrative_ontology:measurement(remo_be_t1756, remonstrance_authority__magistrate_reading, base_extractiveness, 1756, 0.6).
narrative_ontology:measurement(remo_be_t1763, remonstrance_authority__magistrate_reading, base_extractiveness, 1763, 0.64).
narrative_ontology:measurement(remo_be_t1771, remonstrance_authority__magistrate_reading, base_extractiveness, 1771, 0.55).
narrative_ontology:measurement(remo_be_t1776, remonstrance_authority__magistrate_reading, base_extractiveness, 1776, 0.67).
narrative_ontology:measurement(remo_be_t1783, remonstrance_authority__magistrate_reading, base_extractiveness, 1783, 0.71).
narrative_ontology:measurement(remo_be_t1787, remonstrance_authority__magistrate_reading, base_extractiveness, 1787, 0.75).
narrative_ontology:measurement(remo_be_t1789, remonstrance_authority__magistrate_reading, base_extractiveness, 1789, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(remo_su_t1750, remonstrance_authority__magistrate_reading, suppression_requirement, 1750, 0.42).
narrative_ontology:measurement(remo_su_t1756, remonstrance_authority__magistrate_reading, suppression_requirement, 1756, 0.5).
narrative_ontology:measurement(remo_su_t1763, remonstrance_authority__magistrate_reading, suppression_requirement, 1763, 0.57).
narrative_ontology:measurement(remo_su_t1771, remonstrance_authority__magistrate_reading, suppression_requirement, 1771, 0.86).
narrative_ontology:measurement(remo_su_t1776, remonstrance_authority__magistrate_reading, suppression_requirement, 1776, 0.63).
narrative_ontology:measurement(remo_su_t1783, remonstrance_authority__magistrate_reading, suppression_requirement, 1783, 0.7).
narrative_ontology:measurement(remo_su_t1787, remonstrance_authority__magistrate_reading, suppression_requirement, 1787, 0.81).
narrative_ontology:measurement(remo_su_t1789, remonstrance_authority__magistrate_reading, suppression_requirement, 1789, 0.84).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(remonstrance_authority__magistrate_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(remonstrance_authority__magistrate_reading, remonstrance_authority__crown_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'the remonstrance right of the parlements' decomposes, per the ε-invariance principle, into two structurally distinct claims sharing one referent arrangement — this magistrate_reading (gate as fundamental constitutional mechanism; ε reading-indexed to the magistrate seat; victims are commoner taxpayers and episodically the overridden courts) and remonstrance_authority__crown_reading (gate as illegitimate minoritarian veto; its own ε over the identical referent; victims are the crown and the reform program). Neither reading is strictly upstream: each cites the other's explanatory failures as evidence — the crown reading treats the magistrate reading's protective record as proof of captured function, giving this reading downstream structural pressure on the sibling's legitimacy conditions. The two files are linked here; further decomposition of the surrounding privilege economy (venal office property, fiscal exemption regimes) would extend the family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
