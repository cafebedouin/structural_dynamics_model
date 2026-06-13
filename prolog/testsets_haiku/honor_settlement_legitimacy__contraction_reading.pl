% ============================================================================
% CONSTRAINT STORY: honor_settlement_legitimacy__contraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_settlement_legitimacy__contraction_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: honor_settlement_legitimacy__contraction_reading
 *   human_readable: Honor Settlement Legitimacy (Contraction Reading)
 *   domain: legal/cultural/historical
 *
 * SUMMARY:
 *   Under the contraction reading, honor culture's decline is not primarily a
 *   story of legal prohibition or enforcement intensification, but of
 *   cognitive framework transformation. The normative possibility space that
 *   made dueling intelligible as a legitimate action contracted over four
 *   centuries through cultural reframing by intellectuals, literary figures,
 *   and the bourgeoisie. By 1850, dueling had become not merely illegal but
 *   unthinkable — the cognitive vocabulary to justify it had exited the
 *   shared discourse. This reading emphasizes that the constraint operates as
 *   a naturalized cultural logic, not an enforced rule. Beneficiaries include
 *   the commercial bourgeoisie (whose market logic now dominates dispute
 *   resolution) and the state legal authority (whose monopoly on legitimate
 *   violence expands not through force but through the absence of
 *   intelligible alternatives). The low extractiveness and suppression
 *   metrics reflect that this constraint functions like a mountain — it
 *   appears as the natural evolution of culture rather than a constructed
 *   extraction mechanism — yet the beneficiary declarations open it to
 *   false-summit evaluation.
 *
 * KEY AGENTS:
 *   - honor_culture_adherents: historical population whose cognitive framework for legitimate action is contracted out of existence
 *   - state_legal_authority: institutional agenda-setter whose monopoly expands as alternative frameworks become unthinkable
 *   - commercial_bourgeoisie: organized beneficiary whose economic logic becomes the only intelligible frame
 *   - literary_intellectual_class: organized agenda-setter whose narrative reframing drives the cognitive contraction
 *   - women_excluded_from_dueling: permanently structurally excluded; their non-participation is normalized, not liberated, by the framework shift
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_settlement_legitimacy__contraction_reading, 0.18).
domain_priors:suppression_score(honor_settlement_legitimacy__contraction_reading, 0.12).
domain_priors:theater_ratio(honor_settlement_legitimacy__contraction_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_settlement_legitimacy__contraction_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(honor_settlement_legitimacy__contraction_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(honor_settlement_legitimacy__contraction_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_settlement_legitimacy__contraction_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(honor_settlement_legitimacy__contraction_reading, resistance, 0.04).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_settlement_legitimacy__contraction_reading, mountain).
narrative_ontology:human_readable(honor_settlement_legitimacy__contraction_reading, "Honor Settlement Legitimacy (Contraction Reading)").
narrative_ontology:topic_domain(honor_settlement_legitimacy__contraction_reading, "legal/cultural/historical").

domain_priors:emerges_naturally(honor_settlement_legitimacy__contraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_settlement_legitimacy__contraction_reading, 'f109ae55-6161-4ec6-92a2-faaa053ced10').
narrative_ontology:cs_kernel_codification('f109ae55-6161-4ec6-92a2-faaa053ced10', distributed).
narrative_ontology:cs_authority_grounding('f109ae55-6161-4ec6-92a2-faaa053ced10', extraction).
narrative_ontology:cs_interpretation_layer_present('f109ae55-6161-4ec6-92a2-faaa053ced10').
narrative_ontology:cs_reading_relation('f109ae55-6161-4ec6-92a2-faaa053ced10', honor_settlement_legitimacy__drop_reading, coexists_with).
narrative_ontology:cs_reading_relation('f109ae55-6161-4ec6-92a2-faaa053ced10', honor_settlement_legitimacy__composite_reading, influences).
narrative_ontology:cs_axiom('f109ae55-6161-4ec6-92a2-faaa053ced10', foundational, honor_culture_cognitive_extinction).
narrative_ontology:cs_axiom_status(honor_culture_cognitive_extinction, holdable).
narrative_ontology:cs_axiom_grounding('f109ae55-6161-4ec6-92a2-faaa053ced10', honor_culture_cognitive_extinction, empirically_contingent).
narrative_ontology:cs_axiom('f109ae55-6161-4ec6-92a2-faaa053ced10', foundational, rationalist_framework_inevitability).
narrative_ontology:cs_axiom_status(rationalist_framework_inevitability, holdable).
narrative_ontology:cs_axiom_grounding('f109ae55-6161-4ec6-92a2-faaa053ced10', rationalist_framework_inevitability, deontological).
narrative_ontology:cs_reference_frame('f109ae55-6161-4ec6-92a2-faaa053ced10', honor_dispute_settlement_legitimacy).
narrative_ontology:cs_drift_state('f109ae55-6161-4ec6-92a2-faaa053ced10', post_intellectual_reframing_era, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('f109ae55-6161-4ec6-92a2-faaa053ced10', '').
narrative_ontology:cs_kernel_id(honor_settlement_legitimacy__contraction_reading, honor_settlement_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_settlement_legitimacy__contraction_reading, commercial_bourgeoisie).
narrative_ontology:constraint_beneficiary(honor_settlement_legitimacy__contraction_reading, state_legal_authority).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(honor_settlement_legitimacy__contraction_reading, literary_intellectual_class).
narrative_ontology:constraint_vindicates(honor_settlement_legitimacy__contraction_reading, individual_rational_choice_supremacy).
narrative_ontology:constraint_vindicates(honor_settlement_legitimacy__contraction_reading, market_logic_universality).
narrative_ontology:constraint_vindicates(honor_settlement_legitimacy__contraction_reading, state_monopoly_on_legitimate_violence).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Historical population whose normative framework treated dueling as a legitimate resolution mechanism for insult and honor violation. By the interval's end, this population's cognitive framework has shifted such that dueling is no longer intelligible as legitimate action — not merely prohibited but unthinkable within the now-dominant cultural logic.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__contraction_reading, honor_culture_adherents, observer,
    powerless, biographical, analytical, local).

% Monopolizes legitimate violence and adjudicates property and personal disputes through law courts. Enforces prohibitions on dueling through criminal sanction. Over the interval, the state's authority to define legitimate remedy expands as honor-based settlement frameworks contract — not because enforcement intensifies, but because the conceptual vocabulary that makes dueling intelligible exits the shared normative space.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__contraction_reading, state_legal_authority, agenda_setter,
    institutional, generational, arbitrage, national).

% Benefits from the contraction of honor culture because market logic and rational calculation become the only intelligible frame for dispute resolution. Honor-based remedies are time-consuming, unpredictable, and incompatible with commercial contract enforcement. The shift to state-administered law aligned with bourgeois economic interests — not through explicit extraction but through cognitive reorganization that makes their preferred dispute-resolution logic seem natural.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__contraction_reading, commercial_bourgeoisie, beneficiary,
    organized, generational, arbitrage, national).

% Produces the cultural narratives (novels, essays, philosophical tracts) that reframe honor-culture logic as barbaric, irrational, or primitive. Authors and intellectuals do not directly enforce the prohibition but shape the discourse such that dueling becomes narratively unintelligible to the educated classes — a cognitive reframing that precedes and sustains the legal prohibition.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__contraction_reading, literary_intellectual_class, agenda_setter,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(honor_settlement_legitimacy__contraction_reading, literary_intellectual_class, beneficiary).

% Alternative legitimacy frameworks (family arbitration, church mediation, merchant guild arbitration) that once competed with dueling as settlement mechanisms are also cognitively contracted — not explicitly suppressed but rendered unthinkable as authoritative by the same cultural shift that makes dueling incomprehensible.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__contraction_reading, rival_honor_settlement_mechanisms, excluded,
    powerless, biographical, trapped, local).

% Structurally excluded from participation in honor dueling throughout the interval. Their exit from the honor-culture framework is not a transition but an extraction: they were never inside the legitimate participant class, so the contraction of honor culture does not liberate them — it merely normalizes their permanent structural exclusion from certain forms of recognized dispute resolution.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__contraction_reading, women_excluded_from_dueling, observer,
    powerless, biographical, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Honor settlement legitimacy coordinated dispute resolution without state machinery: an aggrieved party could appeal to shared honor norms and demand satisfaction through combat, with socially recognized ritual and witnesses. This solved the collective action problem of how personal insults are remedied when no centralized authority exists to adjudicate them.
% TRANSFER_FUNCTION: Moves the monopoly on legitimate dispute-resolution from distributed honor culture (decentralized, norm-based, combat-resolved) to centralized state law (monopolized, procedure-based, court-adjudicated). The transfer is not of money or labor but of cognitive legitimacy: what counts as a valid reason for action, what remedy is intelligible, what authority can settle a claim.
% ABSENT_VOICES: Honor-culture adherents would defend dueling as the only honorable remedy for certain insults — but by the interval's end, these voices are not merely excluded from policy conversation, they have become conceptually unthinkable within the shared discourse. Family and merchant arbitrators would advocate for continued alternative settlement mechanisms, but these too are rendered cognitively illegitimate by the same framework shift.
% DISAPPEARANCE_RATIONALE: If the contraction of honor culture and its cognitive unintelligibility were somehow reversed — if dueling became thinkable again as a legitimate remedy — the entire structure of personal dispute resolution would reorganize. Insult remedies would no longer flow through courts; state authority to adjudicate honor would collapse; the bourgeois legal apparatus would lose legitimacy in vast domains of personal injury. The cognitive framework is not decorative — its contraction is constitutive of modern state-monopoly dispute resolution.
% FOUNDING_PROBLEM: Across Europe in the 15th–17th centuries, honor culture provided the only socially recognized mechanism for remedying personal insults and violations of reputation. Dueling solved the coordination problem: when a man's honor was violated, everyone knew what remedy was legitimate and what ritual would settle it. The state had no monopoly on legitimate violence or adjudication; honor culture filled the gap.
% FOUNDING_PROBLEM_CORROBORATION: Historians (Kiernan, Nye, Billacois) document the 15th–17th century prevalence of dueling as the sole legitimate honor remedy and the absence of effective state mechanisms for personal-injury adjudication. By the 18th century, state legal systems had developed and were competing for jurisdiction. Bourgeois economic actors (merchants, manufacturers, bankers) documented in their own writings that honor dueling was incompatible with commercial activity. Literary figures (Voltaire, Montesquieu, later novelists) produced narratives depicting honor dueling as barbaric and irrational. Legal historians confirm the founding problem was genuine and is now resolved — no modern Western state relies on dueling as a dispute-resolution mechanism, and the cognitive vocabulary to make it intelligible has contracted to near zero.
narrative_ontology:disappearance_verdict(honor_settlement_legitimacy__contraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(honor_settlement_legitimacy__contraction_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_settlement_legitimacy__contraction_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(honor_settlement_legitimacy__contraction_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_settlement_legitimacy__contraction_reading_tests).

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(honor_settlement_legitimacy__contraction_reading, ExtMetricName, E),
    domain_priors:suppression_score(honor_settlement_legitimacy__contraction_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(honor_settlement_legitimacy__contraction_reading),
    narrative_ontology:constraint_metric(honor_settlement_legitimacy__contraction_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(honor_settlement_legitimacy__contraction_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(honor_settlement_legitimacy__contraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The contraction reading models a constraint that operates through cognitive reorganization rather than coercive enforcement. Extractiveness is low (0.18) because the constraint does not extract material value or labor — it reorganizes what counts as legitimate action. Suppression is correspondingly low (0.12) because the constraint's persistence does not depend on active coercion once the cognitive framework has shifted; by 1850, dueling is prosecuted almost as a relic, not as a live threat requiring constant enforcement. Theater is minimal (0.08) because the constraint's function is not performative — the cultural logic is genuinely internalized by the educated classes. Accessibility collapse is very high (0.92) because once the honor-culture framework contracts, alternatives to state-law dispute resolution become literally unthinkable; the collapse is cognitive, not structural. Resistance is very low (0.04) because there is no organized movement to restore honor dueling by 1850 — the cultural framework that made it intelligible is gone. The measurement series shows gradual increase in all three metrics over the interval as the state legal system solidifies, literary attacks on dueling accumulate, and bourgeois commerce expands. The slight increases in suppression_requirement reflect the state's periodic need to prosecute residual honor duelers, but the trend is toward dormancy, not hardening enforcement.
 *
 * PERSPECTIVAL GAP:
 *   The contraction reading produces radically different seat perceptions. For the literary intellectual class and state legal authority, the decline of honor dueling is a story of civilizational progress — an escape from barbarism to rationality. For honor-culture adherents (increasingly a residual population by 1850), the same process is a story of cognitive colonization — their entire normative vocabulary for legitimate action has been rendered unintelligible and replaced without their consent. The engine would compute these divergent perceptions from the structural data: the intellectual class sits as beneficiary with mobile exit and organized power; honor-culture adherents sit as targets with trapped exit and powerless position. The widening gap in their experienced constraint types (beneficiary perception of mountain; target perception of snare) is the engine's measure of the cultural domination the contraction reading describes.
 *
 * DIRECTIONALITY LOGIC:
 *   The state legal authority and commercial bourgeoisie are structural beneficiaries (d near 0.0) — the contraction of honor culture creates the cognitive space for their preferred dispute-resolution mechanisms and expands their legitimacy. Honor-culture adherents are the targets (d near 1.0) — their cognitive framework is contracted out of the possibility space without their agency in the process. Literary intellectuals are dual-positioned: they set the agenda for cultural reframing (agenda-setter role) and benefit from the elevation of rational, discourse-based dispute resolution over combat-based resolution (beneficiary role). Women are analytically positioned (observer role) because their exclusion from honor dueling is structural and unchanged by the framework shift — they are not liberated by the contraction of honor culture, merely normalized in their permanent exclusion. The directionality structure reflects that this constraint operates through cognitive reorganization of the entire normative field, not through material extraction from a fixed set of actors.
 *
 * MANDATROPHY ANALYSIS:
 *   The contraction reading avoids a mandate trap that would arise under alternative readings. Under the drop reading (dueling persisted as fringe practice), the mandate would be 'maintain honor culture as a legitimate dispute-resolution option' — but this mandate is dead by 1850 and no institutional actor preserves it, creating a piton: theater without function. Under the composite reading (multiple overdetermined mechanisms), the mandate would be unclear: is it legal prohibition, economic transformation, cultural reframing, or some combination? The contraction reading grounds the mandate precisely: maintain the cognitive framework that makes dueling intelligible as legitimate action. This mandate is unambiguously dead by 1850. The constraint persists, but not because anyone maintains the mandate — it persists because the entire cultural field has reorganized such that honor dueling is no longer thinkable, even by residual adherents. This is not mandate preservation; it is mandate extinction coupled with cognitive lock-in. The low theater ratio (0.08) and complete absence of organized beneficiary defense of the old mandate confirm this reading.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_vs_constructed_cognitive_shift,
    'Is the contraction of honor culture a natural evolution of human thought toward rationality, or a constructed replacement of one legitimacy framework with another?',
    'Comparative ethnography and historical analysis examining whether: (a) societies that underwent different economic and intellectual transformations show different frameworks for dispute resolution, and (b) the bourgeois/rational framework was deliberately promoted or was organic emergence. Examine whether intellectual attacks on dueling cited empirical evidence or merely asserted superiority of rationalist logic.',
    'If natural evolution: the constraint approaches a genuine mountain — the contraction is inevitable and rational actors will always prefer centralized law to combat. If constructed: the constraint is a false summit — honor culture was replaced by state monopoly through cultural domination, not rational discovery. The false-summit reading would require examining whether bourgeois economic logic is actually more rational or merely more powerful.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_vs_constructed_cognitive_shift, conceptual, 'Whether the cognitive framework shift represents natural rational progress or constructed cultural domination').

omega_variable(
    beneficiary_identification_ambiguity,
    'Do commercial bourgeoisie and state legal authority genuinely benefit from the contraction of honor culture, or does the contraction create the appearance of benefit by making their preferred logic seem inevitable?',
    'Historical analysis of material outcomes: did merchants, lawyers, and state officials accumulate economic or political power as a result of the framework shift? Or does the framework shift merely legitimize power structures that emerged for independent economic reasons? Examine whether the beneficiaries would have succeeded anyway through legal prohibition and enforcement, or whether cognitive contraction was structurally necessary.',
    'If genuine benefit: the beneficiary declarations are accurate and the constraint rides on real interests. If appearance only: the ''beneficiaries'' are actually the agenda-setters imposing a framework that benefits them through naturalization. This would strengthen the false-summit signal and suggest the constraint should be reclassified as snare or tangled_rope depending on coercion mechanisms.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_identification_ambiguity, empirical, 'Whether declared beneficiaries actually benefit or merely benefit from naturalization of their preferred framework').

omega_variable(
    cognitive_contraction_mechanism_specificity,
    'What is the precise mechanism by which honor-culture cognitive frameworks contracted? Was it literacy and print culture, economic incentive to adopt rational calculation, conscious intellectual attacks, or demographic replacement of adherents?',
    'Historical analysis of: (a) temporal sequence of literacy expansion, intellectual polemics, and dueling decline; (b) whether individuals converted from honor-culture to legal-system frameworks or were replaced by generations socialized into the new framework; (c) whether the contraction correlates more strongly with intellectual production, economic transformation, or demographic factors.',
    'If mechanism is intellectual/discursive: the constraint is a story about narrative power and what gets published. If mechanism is economic: the constraint is a story about incentive structures overriding cultural preference. If mechanism is demographic: the constraint is a story about generational succession. Different mechanisms would imply different vulnerability points and different classifications under alternative readings.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cognitive_contraction_mechanism_specificity, empirical, 'The specific causal mechanism driving cognitive contraction of honor culture').

omega_variable(
    women_and_excluded_populations_extraction,
    'Does the contraction of honor culture liberate women and non-elite populations from exclusion, or does it merely normalize their permanent exclusion within the new legal framework?',
    'Historical analysis of: (a) whether women''s access to dispute resolution improved after the contraction of honor culture and expansion of state law; (b) whether non-elite populations gained or lost remedy options when honor arbitration and merchant guilds contracted; (c) whether the state legal system that replaced honor culture was more or less accessible to women and the poor.',
    'If liberation: the contraction benefits excluded populations by opening access to state legal remedies. If normalization of exclusion: the contraction is a form of extraction — it eliminates alternative mechanisms (arbitration, mediation, family settlement) that women and the poor may have used, replacing them with a state monopoly less accessible to those without resources. This would reframe women and non-elites as victims of the constraint, not neutral observers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(women_and_excluded_populations_extraction, empirical, 'Whether cognitive contraction of honor culture liberates or excludes non-elite populations').

omega_variable(
    reading_vs_sibling_empirical_discrimination,
    'What empirical evidence would distinguish the CONTRACTION reading from the COMPOSITE reading? Both predict the end of dueling and cognitive shift, but they differ in: (1) whether the shift is unified or fragmentary, (2) whether it is reversible or locked-in, (3) whether it is driven by one mechanism or overdetermined by many.',
    'Historical analysis of: (a) whether dueling''s decline was rapid and unified across all social classes, or gradual and fragmented (composite prediction); (b) whether residual honor-culture advocates remain present in 19th-century discourse arguing for dueling''s legitimacy, or whether the vocabulary to make such arguments has entirely exited the discourse (contraction prediction); (c) whether the constraint''s persistence after 1850 is maintained by active enforcement or passive cognitive lock-in (contraction prediction) versus continued coercive suppression (composite prediction).',
    'High empirical discrimination value: the contraction reading''s strength rests on the claim that honor culture became literally unthinkable, not merely illegal. The composite reading claims multiple mechanisms overdetermined the outcome. Clear evidence of residual honor-culture advocates in 19th-century discourse, or of continued intensive state enforcement, would weaken the contraction reading and strengthen the composite reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_vs_sibling_empirical_discrimination, empirical, 'Empirical discrimination between contraction and composite readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_settlement_legitimacy__contraction_reading, 1450, 1850).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hono_tr_t1450, honor_settlement_legitimacy__contraction_reading, theater_ratio, 1450, 0.02).
narrative_ontology:measurement_basis(hono_tr_t1450, projected).
narrative_ontology:measurement(hono_tr_t1550, honor_settlement_legitimacy__contraction_reading, theater_ratio, 1550, 0.03).
narrative_ontology:measurement_basis(hono_tr_t1550, observed).
narrative_ontology:measurement(hono_tr_t1650, honor_settlement_legitimacy__contraction_reading, theater_ratio, 1650, 0.05).
narrative_ontology:measurement_basis(hono_tr_t1650, observed).
narrative_ontology:measurement(hono_tr_t1750, honor_settlement_legitimacy__contraction_reading, theater_ratio, 1750, 0.07).
narrative_ontology:measurement_basis(hono_tr_t1750, observed).
narrative_ontology:measurement(hono_tr_t1850, honor_settlement_legitimacy__contraction_reading, theater_ratio, 1850, 0.08).
narrative_ontology:measurement_basis(hono_tr_t1850, observed).

% Extraction over time
narrative_ontology:measurement(hono_be_t1450, honor_settlement_legitimacy__contraction_reading, base_extractiveness, 1450, 0.08).
narrative_ontology:measurement_basis(hono_be_t1450, projected).
narrative_ontology:measurement(hono_be_t1550, honor_settlement_legitimacy__contraction_reading, base_extractiveness, 1550, 0.12).
narrative_ontology:measurement_basis(hono_be_t1550, observed).
narrative_ontology:measurement(hono_be_t1650, honor_settlement_legitimacy__contraction_reading, base_extractiveness, 1650, 0.15).
narrative_ontology:measurement_basis(hono_be_t1650, observed).
narrative_ontology:measurement(hono_be_t1750, honor_settlement_legitimacy__contraction_reading, base_extractiveness, 1750, 0.17).
narrative_ontology:measurement_basis(hono_be_t1750, observed).
narrative_ontology:measurement(hono_be_t1850, honor_settlement_legitimacy__contraction_reading, base_extractiveness, 1850, 0.18).
narrative_ontology:measurement_basis(hono_be_t1850, observed).

% Suppression requirement over time
narrative_ontology:measurement(hono_su_t1450, honor_settlement_legitimacy__contraction_reading, suppression_requirement, 1450, 0.02).
narrative_ontology:measurement_basis(hono_su_t1450, projected).
narrative_ontology:measurement(hono_su_t1550, honor_settlement_legitimacy__contraction_reading, suppression_requirement, 1550, 0.04).
narrative_ontology:measurement_basis(hono_su_t1550, observed).
narrative_ontology:measurement(hono_su_t1650, honor_settlement_legitimacy__contraction_reading, suppression_requirement, 1650, 0.07).
narrative_ontology:measurement_basis(hono_su_t1650, observed).
narrative_ontology:measurement(hono_su_t1750, honor_settlement_legitimacy__contraction_reading, suppression_requirement, 1750, 0.1).
narrative_ontology:measurement_basis(hono_su_t1750, observed).
narrative_ontology:measurement(hono_su_t1850, honor_settlement_legitimacy__contraction_reading, suppression_requirement, 1850, 0.12).
narrative_ontology:measurement_basis(hono_su_t1850, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_settlement_legitimacy__contraction_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(honor_settlement_legitimacy__contraction_reading, 0.12).
narrative_ontology:affects_constraint(honor_settlement_legitimacy__contraction_reading, honor_settlement_legitimacy__drop_reading).
narrative_ontology:affects_constraint(honor_settlement_legitimacy__contraction_reading, honor_settlement_legitimacy__composite_reading).
narrative_ontology:affects_constraint(honor_settlement_legitimacy__contraction_reading, state_monopoly_on_legitimate_violence).
narrative_ontology:affects_constraint(honor_settlement_legitimacy__contraction_reading, bourgeois_rationality_universalization).

% DUAL FORMULATION NOTE:
% The honor_settlement_legitimacy kernel contains three distinct readings: contraction_reading (this constraint, where honor culture becomes unthinkable), drop_reading (where honor dueling persists as marginal practice), and composite_reading (where multiple overdetermined mechanisms drive the outcome). The contraction reading is the upstream epistemic claim — it asserts what cognitive transformation occurred — while the other readings disagree about the persistence and mechanism of that transformation. All three readings instantiate the same historical kernel: the shift from honor-based to state-law-based dispute resolution. The contraction reading is the most extractive of the three and the only one that straightforwardly classifies as mountain — the other readings emphasize coercive or mechanical factors that might push the classification toward snare or tangled_rope.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
