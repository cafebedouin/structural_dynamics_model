% ============================================================================
% CONSTRAINT STORY: remonstrance_authority__crown_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_remonstrance_authority__crown_reading, []).

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
 *   constraint_id: remonstrance_authority__crown_reading
 *   human_readable: Droit de Remontrance as Illegitimate Minoritarian Veto (Crown Reading)
 *   domain: constitutional_history/political_economy/legal_authority
 *
 * SUMMARY:
 *   Eighteenth-century France: the sovereign courts (parlements) hold the
 *   right to register, amend, or suspend royal edicts, and they exercise a
 *   suspensive remonstrance against fiscal innovations that touch exempt
 *   interests. This file authors the CROWN READING of that arrangement: the
 *   remonstrance right as an illegitimate minoritarian veto operated by a
 *   small, self-perpetuating, tax-exempt judicial corporation, protecting
 *   particularist privileges (order exemptions, pays d'etat contracts, office
 *   patrimony) at the expense of royal fiscal capacity and the unprivileged
 *   taxpayer. Per the committer-frame rules, the sibling magistrate_reading
 *   (remonstrance as fundamental constitutional mechanism preserving ancient
 *   liberties) is a DIFFERENT constraint in a separate file — it is not
 *   described here, not hedged against, and not averaged with. Claim and
 *   metrics are independently authored facts: the claimed_type (snare) states
 *   this reading's structural belief; the metric values state this reading's
 *   descriptive assessment of the arrangement's actual operation; the engine
 *   computes per-seat classifications from the structural data, and any
 *   divergence between seats is the measurement the corpus exists to take.
 *
 * KEY AGENTS:
 *   - venal_parlementaire_officeholders — agenda-setter/beneficiary (institutional/identity_locked): operates the registration gate; collects office patrimony and exemption protection through it
 *   - royal_fiscal_authority — primary target (institutional/constrained): bears blocked revenue, delay costs, and war-debt risk
 *   - tax_exempt_noble_and_clerical_orders — pure beneficiary (powerful/identity_locked): collects exemption protection without operating the veto
 *   - protected_pays_detat_estates — beneficiary (organized/constrained): defends contracted tax rates through the courts' remonstrances
 *   - unprivileged_taille_paying_commoners — secondary target (powerless/trapped): absorbs the burden shifted by preserved exemptions
 *   - royal_intendants_and_ministers — excluded voice (organized/mobile): argues for direct commission administration, kept outside the registration bargain
 *   - constitutional_historians — analytical observer: reconstructs both the guardianship record and the exemption-defense record
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(remonstrance_authority__crown_reading, 0.85).
domain_priors:suppression_score(remonstrance_authority__crown_reading, 0.66).
domain_priors:theater_ratio(remonstrance_authority__crown_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(remonstrance_authority__crown_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(remonstrance_authority__crown_reading, suppression_requirement, 0.66).
narrative_ontology:constraint_metric(remonstrance_authority__crown_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(remonstrance_authority__crown_reading, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(remonstrance_authority__crown_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(remonstrance_authority__crown_reading, snare).
narrative_ontology:human_readable(remonstrance_authority__crown_reading, "Droit de Remontrance as Illegitimate Minoritarian Veto (Crown Reading)").
narrative_ontology:topic_domain(remonstrance_authority__crown_reading, "constitutional_history/political_economy/legal_authority").

domain_priors:requires_active_enforcement(remonstrance_authority__crown_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(remonstrance_authority__crown_reading, 'ee553495-bb89-4fff-862e-8c622914d7d8').
narrative_ontology:cs_kernel_codification('ee553495-bb89-4fff-862e-8c622914d7d8', distributed).
narrative_ontology:cs_authority_grounding('ee553495-bb89-4fff-862e-8c622914d7d8', lineage).
narrative_ontology:cs_interpretation_layer_present('ee553495-bb89-4fff-862e-8c622914d7d8').
narrative_ontology:cs_reading_relation('ee553495-bb89-4fff-862e-8c622914d7d8', remonstrance_authority__magistrate_reading, forecloses).
narrative_ontology:cs_axiom('ee553495-bb89-4fff-862e-8c622914d7d8', foundational, registered_edicts_bind_without_corporate_suspension).
narrative_ontology:cs_axiom_status(registered_edicts_bind_without_corporate_suspension, holdable).
narrative_ontology:cs_axiom_grounding('ee553495-bb89-4fff-862e-8c622914d7d8', registered_edicts_bind_without_corporate_suspension, conventional).
narrative_ontology:cs_axiom('ee553495-bb89-4fff-862e-8c622914d7d8', secondary, uniform_tax_subjection_is_just_order).
narrative_ontology:cs_axiom_status(uniform_tax_subjection_is_just_order, holdable).
narrative_ontology:cs_axiom_grounding('ee553495-bb89-4fff-862e-8c622914d7d8', uniform_tax_subjection_is_just_order, instrumental).
narrative_ontology:cs_reference_frame('ee553495-bb89-4fff-862e-8c622914d7d8', registration_as_gracious_consultative_act).
narrative_ontology:cs_drift_state('ee553495-bb89-4fff-862e-8c622914d7d8', pre_maupeou_confrontation, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('ee553495-bb89-4fff-862e-8c622914d7d8', '').
narrative_ontology:cs_kernel_id(remonstrance_authority__crown_reading, remonstrance_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(remonstrance_authority__crown_reading, venal_parlementaire_officeholders).
narrative_ontology:constraint_beneficiary(remonstrance_authority__crown_reading, tax_exempt_noble_and_clerical_orders).
narrative_ontology:constraint_beneficiary(remonstrance_authority__crown_reading, protected_pays_detat_estates).
narrative_ontology:constraint_victim(remonstrance_authority__crown_reading, royal_fiscal_authority).
narrative_ontology:constraint_victim(remonstrance_authority__crown_reading, unprivileged_taille_paying_commoners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Purchase sovereign-court offices as hereditary patrimony and decide which royal edicts receive registration. When a fiscal innovation threatens exemption-holding interests, they amend it, delay it through successive secret deliberations, or suspend registration outright, coordinating resistance across the provincial parlements. Their own estates are largely exempt from the direct taxes their suspensions protect. Exit would mean selling an office whose market value collapses the moment the suspensive right is abolished — the office and the right are one asset, so leaving the arrangement means abandoning the family fortune and the robe-nobility self-conception together.
narrative_ontology:constraint_stakeholder(remonstrance_authority__crown_reading, venal_parlementaire_officeholders, agenda_setter,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(remonstrance_authority__crown_reading, venal_parlementaire_officeholders, beneficiary).

% Must raise new revenue faster than legacy taxes yield, especially in wartime. Every fiscal innovation — a new vingtieme, extension of taxation to privileged lands, conversion of annuities — must pass registration, where it can be held up indefinitely. Workarounds exist: compelling registration in a royal session, exiling defiant courts, erecting substitute tribunals. Each carries legitimacy and stability costs large enough that the Crown usually retreats, and the one sustained attempt at abolition (1771) was reversed within three years of the king's death. Bears the accumulating war debt and default risk whenever a broadening reform is blocked.
narrative_ontology:constraint_stakeholder(remonstrance_authority__crown_reading, royal_fiscal_authority, payer,
    institutional, generational, constrained, national).

% Hold exemption from the principal direct taxes as a marker of honor and corporate privilege. They contribute nothing to operating the registration veto but collect its protection continuously: every successful suspension of a universalizing tax preserves their position. Their identity as a legally distinct exempt order dissolves if uniform subjection ever registers, so defending the veto is indistinguishable from defending who they are.
narrative_ontology:constraint_stakeholder(remonstrance_authority__crown_reading, tax_exempt_noble_and_clerical_orders, beneficiary,
    powerful, generational, identity_locked, national).

% Provinces governed under negotiated tax contracts (Brittany, Languedoc, Burgundy) defend lower or quasi-fixed assessments through the courts' remonstrances. They lobby the parlementaires, fund resistance campaigns, and supply the constitutional arguments about provincial liberties. Their stake ends if fiscal uniformity arrives; they have no way to protect their rates except through the courts that hold the registration gate.
narrative_ontology:constraint_stakeholder(remonstrance_authority__crown_reading, protected_pays_detat_estates, beneficiary,
    organized, biographical, constrained, regional).

% Bear the taille and the accumulated deficits that exempted shoulders decline. Every successful veto of a broadening reform deepens their share of a shrinking base. They hold no corporate seat in any sovereign court, cannot register, amend, or suspend anything, and appear only as individual petitioners. Flight, evasion, and under-declaration are the only exits, all costly and all criminal.
narrative_ontology:constraint_stakeholder(remonstrance_authority__crown_reading, unprivileged_taille_paying_commoners, payer,
    powerless, biographical, trapped, national).

% Commissioners administering taxation directly in the generalites. They argue in memoranda for bypassing the courts entirely — levying by commission, collecting without registration — and carry the Crown's institutional memory of what unblocked administration looks like. The registration arrangement excludes them precisely where their expertise applies; they influence policy only at the moments the Crown decides to force a registration.
narrative_ontology:constraint_stakeholder(remonstrance_authority__crown_reading, royal_intendants_and_ministers, excluded,
    organized, biographical, mobile, national).

% Reconstruct the practice from court registers, remonstrance texts, and ministerial correspondence. They can see both the guardianship record (remonstrances catching drafting defects and procedural violations) and the exemption-defense record (remonstrances shielding officeholder and order interests) without holding either position, and they watch the 1771 abolition and 1774 recall as a natural experiment in exit cost.
narrative_ontology:constraint_stakeholder(remonstrance_authority__crown_reading, constitutional_historians, observer,
    analytical, civilizational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(remonstrance_authority__crown_reading, venal_parlementaire_officeholders).
narrative_ontology:fixing_cost_class(remonstrance_authority__crown_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Makes royal law effective across a mosaic of customary jurisdictions: registration converts an edict into locally enforceable law, supplies publicity and a deliberative checkpoint that can catch defective drafting, and gives the realm a single procedural moment at which new law becomes public and operative.
% TRANSFER_FUNCTION: Moves fiscal discretion from the Crown to the sovereign courts, and through them to the privileged orders whose exemptions survive each blocked reform; moves delay, litigation, and credibility costs onto royal finances; and thereby shifts the tax burden downward onto unprivileged commoners.
% ABSENT_VOICES: Unprivileged taille-paying commoners are absent from every seat of the arrangement — the courts speak in the nation's name but recruit from venal robe families. Rural communities and urban laborers would object to the burden their exclusion produces; they appear only as petitioners, never as parties. Royal commissioners with direct-administration expertise are heard only episodically, when the Crown forces a registration.
% DISAPPEARANCE_RATIONALE: If the suspensive veto vanished overnight, the registration bottleneck disappears: the Crown legislates toward fiscal uniformity, exemptions erode, the courts shrink to judicial function, and the entire fiscal-political equilibrium built on preserved privilege reorganizes. This is approximately what the 1771 abolition began and the Revolution completed — the arrangement's removal rearranges the distribution of taxation, the market for offices, and the balance between center and provinces.
% FOUNDING_PROBLEM: Medieval kingship needed counsel-and-consent machinery for new law in a realm without a codified constitution: registration by the sovereign courts supplied publicity, local adaptation, and a formal channel for the notables' objections, under the maxim that what touches all must be approved by all.
% FOUNDING_PROBLEM_CORROBORATION: Contested from both sides by sources outside the beneficiary set: royal fiscal administrators' memoranda (Controle general papers) and physiocrat publicists attest that the consultative function had decayed into interest defense; Jansenist and patriot pamphleteers, equally outside the beneficiary set, attest that guardianship of fundamental law remains a live necessity. Corroboration exists on both sides and neither side's account is confirmed by the other's opponents — which is itself evidence the status is genuinely disputed rather than settled.
narrative_ontology:disappearance_verdict(remonstrance_authority__crown_reading, world_rearranges).
narrative_ontology:founding_problem_status(remonstrance_authority__crown_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(remonstrance_authority__crown_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(remonstrance_authority__crown_reading, 'none', 1).
narrative_ontology:epsilon_provenance(remonstrance_authority__crown_reading, 0.85, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(remonstrance_authority__crown_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(remonstrance_authority__crown_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(remonstrance_authority__crown_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.62 to 0.85 across the interval: each decade of blocked reform entrenches exemptions further, widens the gap between the tax base the Crown can reach and the one that exists, and compounds war debt. Theater climbs past 0.5 by the 1760s — the guardianship-of-the-laws performance increasingly functions as cover for interest defense, which is precisely the crown reading's charge. The suppression_requirement series is authored deliberately because this story TRACES ENFORCEMENT INTENSIFICATION: suspending registration required progressively more machinery — interdiction of correspondence, coordinated sessions across courts, the 'union of classes' doctrine, and finally a mobilized print-public opinion that raised the cost of every forced registration. Suppression (scalar, unscaled by power or scope per the framework's rule) sits at 0.66: the chokepoint is structural (whoever controls registration controls legislation), not internalized — the Crown never accepted the arrangement as deserved. Accessibility collapse is LOW (0.38) because real alternatives persisted and were repeatedly demonstrated: compelled registration in lit de justice, exiling of defiant courts, and the full 1771 substitution of salaried commissions for venal offices. Resistance is HIGH (0.78) — the arrangement met sustained, escalating pushback from the strongest actor in the realm, which is dispositive evidence that this is a constructed constraint defended by its holders, not a natural feature. Identity-lock dynamics: the venal office fuses asset, career, family strategy, and robe-nobility self-conception into one object; when Maupeou broke exactly that frame in 1771 (offices bought out, courts replaced with salaried commissioners), the arrangement collapsed within months — evidence that identity fusion, not functional indispensability, carried the veto. Cyclical dynamics: the series is a monotonic ratchet, not an oscillation — confrontations (1730s bishopric affair, 1749-56 vingtieme conflicts, 1763-71 Brittany and Jesuit crises) each ended with the exemption structure MORE entrenched than before, so crisis-reconciliation cycles function as accumulation phases for the extraction rather than relief valves.
 *
 * PERSPECTIVAL GAP:
 *   From the payer seats the arrangement computes as enforced extraction with identifiable victims; from the agenda-setter and beneficiary seats the identical structure presents itself as constitutional guardianship and honorable exemption. The engine computes these divergent per-seat classifications from the structural data; this file's claim does not adjudicate between them. The magistrate_reading file authors the opposing seat as a separate epsilon-invariant constraint; comparing the two files' outputs is the designed instrument for reading-indexed classification (OQ-26), and the divergence between them is the datum, not a defect in either.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations drive the derivation: venal_parlementaire_officeholders (agenda-setter, beneficiary, identity_locked) sit near the full-beneficiary end — they run the gate, collect through it, and cannot leave it; tax_exempt_noble_and_clerical_orders likewise near-zero d with identity lock amplifying attachment; protected_pays_detat_estates near-beneficiary but slightly less fused (their protection is contractual, not constitutive). royal_fiscal_authority sits near the full-target end: it bears every blocked reform, its workarounds are costly rather than arbitrage-grade (constrained, not mobile), so effective extraction is amplified rather than damped. unprivileged_taille_paying_commoners are full targets with trapped exit and no coalition venue — the amplification is maximal precisely where capacity to respond is minimal, which is the crown reading's core indictment. No directionality overrides are used: the beneficiary/victim declarations plus exit atoms already produce the correct d values, and the override surface is reserved for cases the derivation misreads.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (publicity, consent-channel, and deliberative checkpoint for new law) is PARTIALLY obsolete: eighteenth-century fiscal administration had outgrown the medieval consent machinery, but the guardianship claims retained live outside corroboration (Jansenist and patriot publics), so founding_problem_status is authored CONTESTED rather than dead — no automatic zombie flag fires, correctly, because the arrangement's obsolescence is itself one of the things the crown and magistrate readings dispute. The classification prevents mislabeling in both directions: from this seat, declaring the arrangement a rope (pure coordination) would launder privilege-protection as deliberation; the omegas guarding the defect-catching share of remonstrance activity keep the snare claim falsifiable — if archival coding showed most remonstrances caught genuine legal defects rather than defending exemptions, the arrangement migrates toward tangled_rope even under the crown's own lights. Mandatrophy_resolved is NOT set: the mandate question is the live dispute the kernel exists to carry.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This file authors only the crown_reading of the remonstrance_authority kernel: is the high extractedness measured here a property of the standing arrangement, or an artifact of assessing it from the Crown''s seat?',
    'Comparative read of the sibling magistrate_reading story against the same referent arrangement; the classification divergence between readings is the intended datum (OQ-26), not an error to be reconciled.',
    'If the sibling computes low extraction with a genuine coordination function, the pair demonstrates reading-indexed epsilon over a fixed referent; neither file alone classifies ''the'' remonstrance right — only the pair does.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Reading-indexed classification of a contested constitutional kernel; epsilon is authored per reading, never averaged across readings.').

omega_variable(
    suspensive_veto_legitimacy_dispute,
    'Is edict registration a constitutional requirement admitting lawful suspensive remonstrance (magistrate premise), or a gracious act of royal will admitting counsel only (crown premise)?',
    'Doctrinal analysis of fundamental-law claims against sovereignty jurisprudence; not resolvable by data alone — it required a framework-level decision, which the Revolution delivered by destroying one framework entire.',
    'This is the located disagreement the readings diverge on: whichever premise holds within a framework, the other is impossible there; the whole classification of the arrangement flips wholesale with it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suspensive_veto_legitimacy_dispute, conceptual, 'The structural element on which the crown and magistrate readings contradict: suspensive veto legitimacy.').

omega_variable(
    guardianship_vs_exemption_defense_share,
    'What share of actual remonstrance activity defended legal coherence or caught defective drafting, versus defending particularist fiscal privilege?',
    'Archival coding of surviving remonstrance texts: proportion of suspensions targeting tax exemptions, office interests, and provincial rate contracts versus proportion addressing procedural or legal defects in the edicts themselves.',
    'A high exemption-defense share confirms the snare claim and the theater trajectory; a high defect-catching share would move the arrangement toward tangled_rope even from the Crown''s seat, since a genuine coordination residue would then be carrying the extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(guardianship_vs_exemption_defense_share, empirical, 'Empirical split of remonstrance activity between guardianship function and privilege defense.').

omega_variable(
    crown_exit_cost_counterfactual,
    'Was abolishing the veto cheap enough for the Crown to sustain (as the 1771 substitution briefly suggested), or prohibitively expensive once succession politics, elite defection, and public opinion are fully counted?',
    'Counterfactual analysis of 1771-1774: stability of the substituted tribunals had Louis XV lived; the price of the 1774 recall as revealed-preference data on what restoration was worth to the successor.',
    'Cheap exit would downgrade suppression and recast the arrangement as fragile coordination awaiting a determined reformer; prohibitive exit sustains the snare reading and the authored fixing_cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(crown_exit_cost_counterfactual, empirical, 'Whether the Maupeou experiment proves the veto''s fixability or its defenders'' depth.').

omega_variable(
    taxpayer_coalition_potential,
    'Could the unprivileged commoners ever have formed a coalition capable of contesting the veto directly, or was their corporate exclusion total throughout the interval?',
    'Analysis of the convocation pathway: the Estates-General of 1789 as the first venue where third-estate weight met the privilege question; survey of earlier assembly proposals and cahier demands for proportional representation.',
    'If coalition formation was structurally impossible before 1789, the victims'' powerlessness is constitutive of the arrangement and the suppression measure understates nothing; if venues existed, the arrangement suppressed an available resistance and suppression is understated.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(taxpayer_coalition_potential, empirical, 'Coalition feasibility for the arrangement''s diffuse victims under corporate representation rules.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(remonstrance_authority__crown_reading, 1715, 1771).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(remo_tr_t1715, remonstrance_authority__crown_reading, theater_ratio, 1715, 0.34).
narrative_ontology:measurement(remo_tr_t1725, remonstrance_authority__crown_reading, theater_ratio, 1725, 0.38).
narrative_ontology:measurement(remo_tr_t1737, remonstrance_authority__crown_reading, theater_ratio, 1737, 0.42).
narrative_ontology:measurement(remo_tr_t1749, remonstrance_authority__crown_reading, theater_ratio, 1749, 0.46).
narrative_ontology:measurement(remo_tr_t1756, remonstrance_authority__crown_reading, theater_ratio, 1756, 0.49).
narrative_ontology:measurement(remo_tr_t1763, remonstrance_authority__crown_reading, theater_ratio, 1763, 0.52).
narrative_ontology:measurement(remo_tr_t1771, remonstrance_authority__crown_reading, theater_ratio, 1771, 0.55).

% Extraction over time
narrative_ontology:measurement(remo_be_t1715, remonstrance_authority__crown_reading, base_extractiveness, 1715, 0.62).
narrative_ontology:measurement(remo_be_t1725, remonstrance_authority__crown_reading, base_extractiveness, 1725, 0.66).
narrative_ontology:measurement(remo_be_t1737, remonstrance_authority__crown_reading, base_extractiveness, 1737, 0.68).
narrative_ontology:measurement(remo_be_t1749, remonstrance_authority__crown_reading, base_extractiveness, 1749, 0.72).
narrative_ontology:measurement(remo_be_t1756, remonstrance_authority__crown_reading, base_extractiveness, 1756, 0.77).
narrative_ontology:measurement(remo_be_t1763, remonstrance_authority__crown_reading, base_extractiveness, 1763, 0.81).
narrative_ontology:measurement(remo_be_t1771, remonstrance_authority__crown_reading, base_extractiveness, 1771, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(remo_su_t1715, remonstrance_authority__crown_reading, suppression_requirement, 1715, 0.4).
narrative_ontology:measurement(remo_su_t1725, remonstrance_authority__crown_reading, suppression_requirement, 1725, 0.44).
narrative_ontology:measurement(remo_su_t1737, remonstrance_authority__crown_reading, suppression_requirement, 1737, 0.48).
narrative_ontology:measurement(remo_su_t1749, remonstrance_authority__crown_reading, suppression_requirement, 1749, 0.53).
narrative_ontology:measurement(remo_su_t1756, remonstrance_authority__crown_reading, suppression_requirement, 1756, 0.58).
narrative_ontology:measurement(remo_su_t1763, remonstrance_authority__crown_reading, suppression_requirement, 1763, 0.62).
narrative_ontology:measurement(remo_su_t1771, remonstrance_authority__crown_reading, suppression_requirement, 1771, 0.66).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(remonstrance_authority__crown_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(remonstrance_authority__crown_reading, remonstrance_authority__magistrate_reading).

% DUAL FORMULATION NOTE:
% Constraint family: remonstrance_authority decomposes into two epsilon-invariant readings of one contested kernel. This file (crown_reading) authors the arrangement from the sovereign's seat — high epsilon, victims include royal fiscal authority and the unprivileged taxpayer, magistrate legitimacy suppressed as obstruction. The sibling (magistrate_reading) authors the same standing arrangement from the courts' seat — guardianship function foregrounded, Crown enters as the arbitrary-innovator. Upstream/downstream: the magistrate reading's fundamental-law doctrine is cited BY the crown reading as the very cover story the veto hides behind, so the sibling's legitimacy claim is the load-bearing input this file attacks; the family link routes contamination analysis between them. Per DP-001, each file carries ONE stable epsilon over the SAME referent arrangement; the pair, not either file, constitutes the classification of the remonstrance right.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
