% ============================================================================
% CONSTRAINT STORY: salic_prohibition__sovereign_override_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_salic_prohibition__sovereign_override_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: salic_prohibition__sovereign_override_reading
 *   human_readable: Salic Prohibition as Sovereign-Override Positive Law
 *   domain: constitutional/dynastic/political
 *
 * SUMMARY:
 *   The Salic prohibition on female succession appears across medieval
 *   European legal codes as a fundamental rule of dynastic order. This
 *   constraint story instantiates ONE reading of the contested kernel: the
 *   reading under which Salic Law is positive law of the realm—revocable by
 *   sovereign legislative authority. Under this reading, the sovereign
 *   retains the power to override Salic prohibition via explicit legislative
 *   act (exemplified historically by the Pragmatic Sanction and similar
 *   decrees), declaring female succession permissible when dynastic
 *   continuity demands it. Challengers to a female heir installed via such
 *   override are treated as rebels against legitimate sovereign authority,
 *   not as defenders of inviolable law. This reading does not claim Salic Law
 *   is wrong or unjust; it claims Salic Law is the sovereign's positive
 *   enactment and therefore subject to the sovereign's power to revise it.
 *   The sibling readings—cognatic_reversion (Salic Law as Frankish
 *   anachronism never binding on other territories) and immutable_mandate
 *   (Salic Law as natural/divine law embedded in dynastic constitution)—are
 *   structurally distinct constraints with different beneficiary sets and
 *   different measurements of extraction. They are not part of this story;
 *   they are linked constraints in the same kernel family.
 *
 * KEY AGENTS:
 *   - reigning_male_monarch: Controls succession machinery; can override Salic prohibition via sovereign act; benefits from doctrine that dynastic continuity justifies override
 *   - male_designated_heir: Protected by Salic Law's presumption; extraction from constraint is the loss of certainty (the sovereign can override to install a female heir)
 *   - eligible_female_claimant: Barred from succession; her claim without sovereign authorization is criminalized as rebellion; extraction is total exclusion from dynastic eligibility
 *   - dynastic_continuity_doctrine: Non-agent entity that benefits from the constraint's flexibility (can be invoked to block unwanted claims or permit wanted ones)
 *   - noble_faction: Dual position—benefits from constraint when it blocks female claims, bears extraction when sovereign uses override against their interests
 *   - legal_establishment: Interprets and propagates the constraint; extracts authority from both enforcement and override machinery
 *   - church_authority: Constrained observer; must ratify sovereign's claim that positive law is revocable without asserting authority over sovereign
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(salic_prohibition__sovereign_override_reading, 0.62).
domain_priors:suppression_score(salic_prohibition__sovereign_override_reading, 0.71).
domain_priors:theater_ratio(salic_prohibition__sovereign_override_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(salic_prohibition__sovereign_override_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(salic_prohibition__sovereign_override_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(salic_prohibition__sovereign_override_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(salic_prohibition__sovereign_override_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(salic_prohibition__sovereign_override_reading, resistance, 0.67).

% --- Constraint claim ---
narrative_ontology:constraint_claim(salic_prohibition__sovereign_override_reading, tangled_rope).
narrative_ontology:human_readable(salic_prohibition__sovereign_override_reading, "Salic Prohibition as Sovereign-Override Positive Law").
narrative_ontology:topic_domain(salic_prohibition__sovereign_override_reading, "constitutional/dynastic/political").

domain_priors:requires_active_enforcement(salic_prohibition__sovereign_override_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(salic_prohibition__sovereign_override_reading, '12feb666-f7f3-4488-b6cd-eb5da313e6ee').
narrative_ontology:cs_kernel_codification('12feb666-f7f3-4488-b6cd-eb5da313e6ee', formalized).
narrative_ontology:cs_authority_grounding('12feb666-f7f3-4488-b6cd-eb5da313e6ee', extraction).
narrative_ontology:cs_interpretation_layer_present('12feb666-f7f3-4488-b6cd-eb5da313e6ee').
narrative_ontology:cs_reading_relation('12feb666-f7f3-4488-b6cd-eb5da313e6ee', salic_prohibition__immutable_mandate_reading, forecloses).
narrative_ontology:cs_reading_relation('12feb666-f7f3-4488-b6cd-eb5da313e6ee', salic_prohibition__cognatic_reversion_reading, influences).
narrative_ontology:cs_axiom('12feb666-f7f3-4488-b6cd-eb5da313e6ee', foundational, sovereign_legislative_supremacy).
narrative_ontology:cs_axiom_status(sovereign_legislative_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('12feb666-f7f3-4488-b6cd-eb5da313e6ee', sovereign_legislative_supremacy, deontological).
narrative_ontology:cs_axiom('12feb666-f7f3-4488-b6cd-eb5da313e6ee', foundational, dynastic_continuity_justifies_override).
narrative_ontology:cs_axiom_status(dynastic_continuity_justifies_override, holdable).
narrative_ontology:cs_axiom_grounding('12feb666-f7f3-4488-b6cd-eb5da313e6ee', dynastic_continuity_justifies_override, instrumental).
narrative_ontology:cs_reference_frame('12feb666-f7f3-4488-b6cd-eb5da313e6ee', sovereign_authority_over_positive_law).
narrative_ontology:cs_drift_state('12feb666-f7f3-4488-b6cd-eb5da313e6ee', post_pragmatic_sanction_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('12feb666-f7f3-4488-b6cd-eb5da313e6ee', '').
narrative_ontology:cs_kernel_id(salic_prohibition__sovereign_override_reading, salic_prohibition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(salic_prohibition__sovereign_override_reading, reigning_male_monarch).
narrative_ontology:constraint_beneficiary(salic_prohibition__sovereign_override_reading, dynastic_continuity_doctrine).
narrative_ontology:constraint_victim(salic_prohibition__sovereign_override_reading, eligible_female_claimants).
narrative_ontology:constraint_victim(salic_prohibition__sovereign_override_reading, legitimacy_challengers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(salic_prohibition__sovereign_override_reading, male_designated_heir).
narrative_ontology:constraint_beneficiary(salic_prohibition__sovereign_override_reading, noble_faction_defending_male_line).
narrative_ontology:constraint_victim(salic_prohibition__sovereign_override_reading, eligible_female_claimant).
narrative_ontology:constraint_victim(salic_prohibition__sovereign_override_reading, noble_faction_defending_male_line).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Controls the succession machinery and can—under this reading—use sovereign legislative authority to override Salic prohibition via instrument like the Pragmatic Sanction. Sets the interpretive frame: Salic Law is positive law of the realm, revocable by legitimate sovereign will, not immutable divine mandate. Defends dynastic continuity through controlled female succession when male line fails, thereby preserving the dynasty's legitimacy claim and the monarch's control over succession outcomes.
narrative_ontology:constraint_stakeholder(salic_prohibition__sovereign_override_reading, reigning_male_monarch, agenda_setter,
    institutional, generational, arbitrage, national).

% Salic rule protects his succession by legal presumption; if Salic Law holds as immutable, his position is unambiguous. Under the sovereign-override reading, his inheritance is secure only so long as the reigning monarch does not invoke the override—his position is protected by the constraint while the monarch reserves discretion.
narrative_ontology:constraint_stakeholder(salic_prohibition__sovereign_override_reading, male_designated_heir, beneficiary,
    powerful, biographical, mobile, national).

% Barred from succession by Salic prohibition in its standard form. Under this reading, her claim is legally void unless the sovereign explicitly revokes Salic Law on her behalf via sovereign act. She bears the cost of the constraint: her dynastic eligibility is erased, and any claim she presses without sovereign authorization is treated as rebellion against legitimate authority. Even with sovereign override, her position depends on the monarch's discretionary choice, not on her own legal standing.
narrative_ontology:constraint_stakeholder(salic_prohibition__sovereign_override_reading, eligible_female_claimant, payer,
    powerful, biographical, trapped, national).

% The claim that dynastic continuity is the supreme constitutional value. Under this reading, female succession is permissible precisely because it preserves the dynasty—the constraint is revocable in service of a higher principle (continuity via sovereign act). The doctrine benefits from the flexibility the reading provides: it can invoke Salic Law to block unwanted female claims, or invoke sovereign override to permit wanted ones, without ceasing to assert dynastic legitimacy as foundational.
narrative_ontology:constraint_stakeholder(salic_prohibition__sovereign_override_reading, dynastic_legitimacy_doctrine, beneficiary,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_non_agent(salic_prohibition__sovereign_override_reading, dynastic_legitimacy_doctrine).

% A female claimant who presses her succession claim without sovereign authorization. Under this reading, she is not a legitimate alternative authority—she is a rebel against the sovereign's authority to decide succession. Her claim is not merely denied; it is criminalized as sedition. She is excluded from the legitimate conversation because her very act of pressing a claim (without sovereign permission) places her outside the law.
narrative_ontology:constraint_stakeholder(salic_prohibition__sovereign_override_reading, rebellious_female_claimant, excluded,
    moderate, biographical, trapped, national).

% Nobles with interests in the male heir's succession bear the cost of the constraint's flexibility: the sovereign can override Salic Law and install a female heir, undermining their prior settlements and expectations. They benefit from the constraint's enforcement when it blocks unwanted female claims, but face extraction when the sovereign uses override to restructure the succession against their interests.
narrative_ontology:constraint_stakeholder(salic_prohibition__sovereign_override_reading, noble_faction_defending_male_line, payer,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(salic_prohibition__sovereign_override_reading, noble_faction_defending_male_line, beneficiary).

% A rival dynastic house or external military power. Under this reading, they observe that the sovereign has declared Salic Law revocable and installed a female heir via Pragmatic Sanction. They can support the female successor's legitimacy (undermining the realm's internal cohesion) or support male-line challengers (prolonging internal conflict and instability). Their analytical position is that the sovereign's override reveals Salic Law was never immutable—it was always a posture of authority that could be revised when sovereign interest demanded it.
narrative_ontology:constraint_stakeholder(salic_prohibition__sovereign_override_reading, foreign_power, observer,
    powerful, biographical, arbitrage, global).

% Jurists, church councils, and codifiers who interpret and propagate the law. Under this reading, they face pressure to frame Salic Law as positive law—revocable by legitimate sovereign will—rather than as immutable natural or divine law. They administer the sovereign's override decrees and provide the legal machinery for reinterpreting succession rules. They extract authority from both the constraint (they interpret it) and from its override (they legitimize the sovereign's legislative power).
narrative_ontology:constraint_stakeholder(salic_prohibition__sovereign_override_reading, legal_establishment, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(salic_prohibition__sovereign_override_reading, legal_establishment, observer).

% Ecclesiastical authority is asked to confirm whether Salic Law is divine mandate or human positive law. Under this reading, church authority's role is constrained: it can ratify the sovereign's claim that positive law is revocable (which would require reframing Salic Law as human enactment), or it can resist by asserting divine immutability (which would challenge sovereign authority itself). Church observers must navigate between divine law and dynastic continuity doctrine.
narrative_ontology:constraint_stakeholder(salic_prohibition__sovereign_override_reading, church_authority, observer,
    institutional, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(salic_prohibition__sovereign_override_reading, reigning_male_monarch).
narrative_ontology:fixing_cost_class(salic_prohibition__sovereign_override_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Stabilizes succession by providing a clear legal rule: males inherit in preference to females, eliminating ambiguity about heir designation and reducing succession disputes. The rule provides common-law expectation that enables nobles, the church, and foreign powers to calibrate their loyalties and military commitments in advance of the monarch's death.
% TRANSFER_FUNCTION: Moves succession authority from female dynastic claimants to male ones, and reserves to the reigning sovereign the discretionary power to override the rule via legislative act. The constraint transfers legitimacy-grant power to the monarch, enabling the monarch to claim authority to revise fundamental succession rules in service of dynastic continuity. It moves exclusion costs to female claimants and uncertainty costs to male heirs (whose succession is now contingent on the sovereign's not invoking override).
% ABSENT_VOICES: Female claimants who are barred are structurally excluded from the succession conversation and any reform of succession law—their position is, under this reading, rebellion rather than legitimate political claim. Male-line challengers from collateral branches lack standing to contest the sovereign's override decision once it is declared—their voices are excluded by the law's own logic (the sovereign's will is supreme). Foreign powers have analytical standing but no formal voice.
% DISAPPEARANCE_RATIONALE: If this constraint (Salic prohibition as revocable positive law) vanished, the realm would face immediate succession ambiguity: female claimants would press claims with equivalent legal standing to males; the sovereign's claimed power to override via positive law would evaporate; nobles, church, and foreign powers would scramble to calibrate loyalty without the legal framework; civil conflict over succession would intensify. The constraint's disappearance would require either replacement with a new succession rule (e.g., open primogeniture regardless of sex) or collapse into hereditary civil war. The world rearranges because succession is constitutive of political order—it cannot be absent without reorganizing the entire legitimacy structure.
% FOUNDING_PROBLEM: Female succession creates ambiguity about whether the dynasty is truly continuous or has ended. If a female inherits, is the dynasty carried forward by her issue (and through what patriline?), or is the female merely a caretaker for a collateral male heir? Salic Law solves this by declaring females ineligible, so male succession is always presumed to carry forward the dynasty without question. The sovereign-override reading adds: the founding problem also includes how to preserve the dynasty when no capable male heir exists—and the sovereign must retain the power to resolve this crisis without being bound by immutable law.
% FOUNDING_PROBLEM_CORROBORATION: The reigning monarch and dynastic continuity theorists attest the founding problem is live: female succession creates legitimacy ambiguity that threatens dynastic stability, and the sovereign must retain override power for cases where the male line fails but dynasty continuity requires a female heir. Immutabilist challengers attest the founding problem is no longer live because Salic Law itself has become the settled rule—revocable override power resurrects the very ambiguity it was supposed to eliminate. Male-line nobles testify (under duress, when override threatens their interests) that override power creates instability: the sovereign's ability to revise succession ex post destabilizes noble investment in the male heir's succession. No corroboration exists from female claimants themselves (they are excluded from the conversation as rebels if they press claims); their testimony would exist only as seditious challenge, not as legitimate voice.
narrative_ontology:disappearance_verdict(salic_prohibition__sovereign_override_reading, world_rearranges).
narrative_ontology:founding_problem_status(salic_prohibition__sovereign_override_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(salic_prohibition__sovereign_override_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(salic_prohibition__sovereign_override_reading, 'none', 1).
narrative_ontology:epsilon_provenance(salic_prohibition__sovereign_override_reading, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(salic_prohibition__sovereign_override_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(salic_prohibition__sovereign_override_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(salic_prohibition__sovereign_override_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.48 to 0.62 over the interval because the sovereign's demonstrated willingness to invoke override (via the Pragmatic Sanction and similar acts) gradually increases the uncertainty for male heirs and the enforcement costs for those defending male-line succession. Theater ratio rises from 0.32 to 0.48 because the constraint's operation increasingly relies on the performance of sovereign authority asserting its right to override—the constraint is less about Salic Law itself and more about the sovereign's power to claim Salic Law is revocable. Suppression requirement rises from 0.58 to 0.71 because maintaining female claimants' exclusion requires active enforcement: legal disqualification of their claims, criminalization of their press for succession, suppression of their supporters' voice. The measurements share a single time grid (every metric is authored at every examined time point, from t=0 to t=40). The rising trajectory reflects the constraint's transformation from a standing rule (early period: Salic Law is simply how succession works) to an actively administered discretionary instrument (later period: the sovereign openly reserves the power to override, which requires suppressing the claim that Salic Law is immutable). Crucially, extractiveness plateaus after t=24, suggesting the constraint reaches a stable state once the sovereign's override power is publicly established and internalized by the legal establishment and nobles—the uncertainty is resolved into a clear hierarchy (sovereign authority supreme over positive law), and the extraction stabilizes at the new level.
 *
 * PERSPECTIVAL GAP:
 *   The reigning monarch and legal establishment compute this constraint very differently from male heirs and noble factions. From the sovereign's seat, the constraint is a flexible coordination tool: it provides the default presumption (males inherit), but the sovereign retains ultimate authority to revise it (override via legislative act). From the male heir's seat, the constraint's flexibility is a source of persistent threat—his inheritance can be revoked by sovereign will; his security depends on the monarch's choices, not on law. From the noble faction's seat, the constraint is extractive: it binds nobles to support a male heir whose succession can be arbitrarily overridden by the sovereign, forcing nobles to re-negotiate their loyalty and land settlements. From the female claimant's seat (excluded), the constraint is total exclusion: not merely denied inheritance but barred from pressing a legal claim (any such press is rebellion). The engine's per-seat computation should surface this: the sovereign seat should compute as coordinating (low extraction, low directionality toward target), the male heir seat as moderately extractive (dependent on sovereign forbearance), noble seats as highly extractive (uncertainty + forced re-negotiation), and female claimant seats as total snare (criminalized claim). The authored claim (tangled_rope) sits between rope (the coordination story) and snare (the extraction reality).
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries: the reigning sovereign (collects override authority + discretionary power), and the dynastic continuity doctrine (vindicated by the constraint's flexibility—female succession preserves dynasty, proving the doctrine's superiority over immutable-law alternatives). Victims: eligible female claimants (total barring from succession; any unsanctioned claim is rebellion), and legitimacy challengers (male-line collateral heirs and external powers are denied standing to contest a sovereign-authorized female succession). The beneficiary directionality is near 0.0 (the sovereign benefits by design; exit options are arbitrage—the sovereign can choose to invoke or not invoke override). The victim directionality for female claimants is near 1.0 (trapped, identity-locked to dynastic exclusion by sex, criminalized if they press claim). The victim directionality for legitimacy challengers is high (~0.8) because their standing is denied by the constraint's own logic (the sovereign's authority is supreme; override decisions are not subject to noble or foreign challenge). The directive logic establishes asymmetric extraction: female claimants and legitimacy challengers bear costs that the sovereign and dynastic doctrine avoid.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (female succession creates dynastic legitimacy ambiguity; the sovereign must retain power to resolve this crisis) is asserted as LIVE by the sovereign and dynastic theorists, but is CONTESTED by challengers who claim Salic Law itself has become the settled solution and that override power resurrects the original problem. The constraint's classification as tangled_rope depends on this contest remaining unresolved: IF the founding problem is live, then female succession IS genuinely coordination (preserving dynasty) and override authority IS necessary (beneficiary + enforcement = tangled_rope). IF the founding problem is dead (Salic Law is settled; dynasties are secure without override), then the constraint becomes pure extraction (mandatrophy, zombie constraint)—the sovereign extracts override authority from a rule that no longer solves any coordination problem. The measurements showing persistent extraction and rising theater support the mandatrophy reading: the constraint's actual operation (rising suppression, stable theater after t=24) suggests the founding problem is being managed theatrically, not functionally. The divergence between claimed_type (tangled_rope, coordination-with-asymmetric-extraction) and the metric profile (persistent extraction, rising theater) signals exactly this mandatrophy risk: the constraint persists as a power instrument even if its original coordination function has atrophied. The immutable_mandate reading challenges mandatrophy by asserting Salic Law is NOT revocable (i.e., override authority is illegitimate, so no crisis-resolution function exists). The cognatic_reversion reading challenges it by asserting Salic Law was never binding (so the constraint itself is anachronistic). This reading accepts mandatrophy risk as structural: the sovereign-override framing necessarily produces it because if the sovereign retains power to override, the constraint is only as stable as the sovereign's current will—and that is not mandatrophy-resistant.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    salic_law_naturalness_ambiguity,
    'Is Salic Law a human positive enactment (revocable by sovereign will) or a natural/divine law embedded in dynastic order (immutable regardless of sovereign preference)?',
    'Textual analysis of foundational codifications (Lex Salica, Frankish compilations) distinguishing positive-law language from natural-law language; historical analysis of origins (was Salic Law consciously enacted or did it emerge from practice?); ecclesiastical and juristic testimony about whether Salic Law is grounded in divinity or human authority.',
    'If Salic Law is natural/divine law, the sovereign-override reading collapses: the sovereign cannot override immutable law, and any attempt to install a female heir via Pragmatic Sanction is illegitimate usurpation. The constraint type shifts from tangled_rope (coordinated with discretionary override) to snare (pure suppression of female claimants, with the sovereign''s override claims being theater for power consolidation). If Salic Law is positive enactment, the sovereign-override reading stands, and the constraint is tangled_rope (coordination + asymmetric extraction). The classification depends entirely on how this ambiguity is resolved.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(salic_law_naturalness_ambiguity, conceptual, 'Whether Salic prohibition is human positive law or immutable natural/divine law').

omega_variable(
    dynastic_continuity_foundation,
    'Does female succession actually threaten dynastic continuity, or is the threat to dynastic legitimacy primarily one of uncertainty and factional dispute (resolvable by other means)?',
    'Comparative historical analysis: examine cases where female succession occurred (Spain, England, Poland-Lithuania, Russia) and measure succession stability, dynastic persistence, and legitimacy challenges compared to male-only succession regimes. Distinguish between female succession itself as a structural threat versus the turbulence caused by factional resistance to female succession.',
    'If female succession does cause dynastic instability, the founding problem is live, and the sovereign''s claim to need override authority is justified—the constraint is coordination (solving a real problem). If female succession is stable (stability depends on other factors: clear designation, noble consensus, church support, military strength), the founding problem is dead—the constraint persists as extraction (the sovereign invokes crisis-need to justify power over succession that is not actually threatened). This omega determines whether the constraint is a genuine tangled_rope or a mandatrophic snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dynastic_continuity_foundation, empirical, 'Whether female succession structurally threatens dynastic continuity or if threat is primarily from factional resistance').

omega_variable(
    sovereign_override_legitimacy_ground,
    'On what basis does the sovereign claim authority to override Salic Law? Is this authority grounded in natural sovereignty (the monarch is beyond law), positive delegation (the realm''s laws grant the monarch revision power), or performative assertion (the monarch claims power and enforces the claim through military/institutional force)?',
    'Analysis of the Pragmatic Sanction and similar override decrees: do they invoke natural sovereignty, positive legal delegation, or pragmatic necessity? Do they claim to be exercising a pre-existing reserved power, or claiming to create a new power de facto? Examine whether the override authority is asserted as consistent with law or as superseding law.',
    'If sovereign authority is grounded in natural law (the monarch is inherently beyond positive law), the constraint is pure power: the sovereign can declare anything, and Salic Law is merely the current expression of sovereign will. If grounded in positive delegation (the realm''s law grants override authority), the constraint is tangled_rope with legitimacy: the override is lawful exercise of lawful authority. If grounded in performative assertion (the monarch claims power and forces recognition), the constraint is snare wearing tangled_rope costume—the sovereignty claim is extraction dressed as authority. This omega determines the legitimacy status of the override mechanism itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sovereign_override_legitimacy_ground, conceptual, 'What grounds the sovereign''s claimed authority to override Salic Law').

omega_variable(
    female_claimant_suppression_mechanism,
    'Is the measured suppression (0.71) structural (legal disqualification, institutional bars to claim-making, military force used against female claimants) or internalized (female claimants themselves believe they are ineligible, and accept the exclusion as just)?',
    'Historical analysis of female succession attempts: do female claimants press claims against active suppression (structural), or do they accept exclusion without pressing claims (internalized)? Examine narratives and testimony: do sources report female claimants as criminalized rebels or as women who naturally defer to male heirs?',
    'If suppression is primarily structural, the constraint''s persistence depends on active enforcement—removal of enforcement machinery would allow female succession claims to emerge immediately. If suppression is internalized, the constraint persists even without active enforcement because female claimants have been taught to not claim. The mix determines whether the constraint is actively maintained (snare requiring constant force) or passively maintained (internalized exclusion), which affects the cost of changing the constraint and the likelihood of its persistence under institutional upheaval.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(female_claimant_suppression_mechanism, empirical, 'Whether suppression of female claimants is structural or internalized').

omega_variable(
    reading_foreclose_test,
    'Does the sovereign-override reading logically foreclose the immutable-mandate reading, or do these readings represent genuinely incommensurable frameworks held by different parties?',
    'Logical analysis: if the sovereign can override Salic Law, is Salic Law immutable? Can both readings be held within a single framework, or do they require the framework itself to change? If both can coexist (e.g., ''Salic Law is immutable except when the sovereign exercises sovereign power to override it''), the readings coexist; if one claim directly contradicts the other (e.g., ''Salic Law is both immutable and revocable'' is incoherent), the readings foreclose.',
    'If the readings foreclose, the kernel contest is a genuine logical contradiction: only one reading can be true in any consistent legal framework. If they coexist, the kernel contest is a contest between frameworks held by different parties (the sovereign asserts override power; immutabilists deny it; both frameworks are internally consistent). The relation type in cs_structure.reading_relations depends on this analysis.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_foreclose_test, conceptual, 'Whether sovereign-override reading logically forecloses the immutable-mandate reading').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(salic_prohibition__sovereign_override_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sali_tr_t0, salic_prohibition__sovereign_override_reading, theater_ratio, 0, 0.32).
narrative_ontology:measurement_basis(sali_tr_t0, observed).
narrative_ontology:measurement(sali_tr_t8, salic_prohibition__sovereign_override_reading, theater_ratio, 8, 0.38).
narrative_ontology:measurement_basis(sali_tr_t8, observed).
narrative_ontology:measurement(sali_tr_t16, salic_prohibition__sovereign_override_reading, theater_ratio, 16, 0.42).
narrative_ontology:measurement_basis(sali_tr_t16, observed).
narrative_ontology:measurement(sali_tr_t24, salic_prohibition__sovereign_override_reading, theater_ratio, 24, 0.47).
narrative_ontology:measurement_basis(sali_tr_t24, observed).
narrative_ontology:measurement(sali_tr_t32, salic_prohibition__sovereign_override_reading, theater_ratio, 32, 0.48).
narrative_ontology:measurement_basis(sali_tr_t32, observed).
narrative_ontology:measurement(sali_tr_t40, salic_prohibition__sovereign_override_reading, theater_ratio, 40, 0.48).
narrative_ontology:measurement_basis(sali_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(sali_be_t0, salic_prohibition__sovereign_override_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(sali_be_t0, observed).
narrative_ontology:measurement(sali_be_t8, salic_prohibition__sovereign_override_reading, base_extractiveness, 8, 0.54).
narrative_ontology:measurement_basis(sali_be_t8, observed).
narrative_ontology:measurement(sali_be_t16, salic_prohibition__sovereign_override_reading, base_extractiveness, 16, 0.59).
narrative_ontology:measurement_basis(sali_be_t16, observed).
narrative_ontology:measurement(sali_be_t24, salic_prohibition__sovereign_override_reading, base_extractiveness, 24, 0.62).
narrative_ontology:measurement_basis(sali_be_t24, observed).
narrative_ontology:measurement(sali_be_t32, salic_prohibition__sovereign_override_reading, base_extractiveness, 32, 0.62).
narrative_ontology:measurement_basis(sali_be_t32, observed).
narrative_ontology:measurement(sali_be_t40, salic_prohibition__sovereign_override_reading, base_extractiveness, 40, 0.62).
narrative_ontology:measurement_basis(sali_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(sali_su_t0, salic_prohibition__sovereign_override_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(sali_su_t0, observed).
narrative_ontology:measurement(sali_su_t8, salic_prohibition__sovereign_override_reading, suppression_requirement, 8, 0.64).
narrative_ontology:measurement_basis(sali_su_t8, observed).
narrative_ontology:measurement(sali_su_t16, salic_prohibition__sovereign_override_reading, suppression_requirement, 16, 0.68).
narrative_ontology:measurement_basis(sali_su_t16, observed).
narrative_ontology:measurement(sali_su_t24, salic_prohibition__sovereign_override_reading, suppression_requirement, 24, 0.7).
narrative_ontology:measurement_basis(sali_su_t24, observed).
narrative_ontology:measurement(sali_su_t32, salic_prohibition__sovereign_override_reading, suppression_requirement, 32, 0.71).
narrative_ontology:measurement_basis(sali_su_t32, observed).
narrative_ontology:measurement(sali_su_t40, salic_prohibition__sovereign_override_reading, suppression_requirement, 40, 0.71).
narrative_ontology:measurement_basis(sali_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(salic_prohibition__sovereign_override_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(salic_prohibition__sovereign_override_reading, 0.12).
narrative_ontology:affects_constraint(salic_prohibition__sovereign_override_reading, salic_prohibition__immutable_mandate_reading).
narrative_ontology:affects_constraint(salic_prohibition__sovereign_override_reading, salic_prohibition__cognatic_reversion_reading).
narrative_ontology:affects_constraint(salic_prohibition__sovereign_override_reading, pragmatic_sanction_decree).
narrative_ontology:affects_constraint(salic_prohibition__sovereign_override_reading, female_succession_legitimacy_crisis).

% DUAL FORMULATION NOTE:
% The SALIC_PROHIBITION kernel decomposes into three structurally distinct constraints, each with its own ε, beneficiary set, and type classification. SOVEREIGN_OVERRIDE_READING (this constraint) asserts Salic Law is positive law revocable by sovereign authority; female succession is permissible via explicit override; challengers are rebels. IMMUTABLE_MANDATE_READING asserts Salic Law is natural/divine law grounded in dynastic constitution; override is impossible; any female succession is illegitimate usurpation. COGNATIC_REVERSION_READING asserts Salic Law was never binding on non-Frankish territories; female succession rights inhere in cognatic inheritance regardless. These readings are not perspectives on the same constraint—they are different constraints with different structural properties. They are linked via network.affects_constraints because they share a kernel (the claim 'Salic Law prohibits female succession') but instantiate different readings of what that claim means and what its implications are.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(salic_prohibition__sovereign_override_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
