% ============================================================================
% CONSTRAINT STORY: family_law_authority__hindu_dharmashastra_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_family_law_authority__hindu_dharmashastra_reading, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: family_law_authority__hindu_dharmashastra_reading
 *   human_readable: Hindu Dharmashastra Marriage Governance
 *   domain: religious/family law/political
 *
 * SUMMARY:
 *   This is the Hindu dharmashastra reading of the contested kernel 'family
 *   law authority.' The constraint embeds marriage in sacramental obligation
 *   (samskara), patrilineal property transmission, caste endogamy, and
 *   brahminical legitimacy oversight. From the dharmashastra reading's
 *   internal standpoint, marriage is a sacred life-stage ritual whose
 *   indissolubility is the price of ritual completeness and property
 *   stability — a natural law of dharma. From the seats of married women,
 *   inter-caste couples, and divorced women, the same structure is a coercive
 *   extraction mechanism that traps them in households, denies them property,
 *   forbids them remarriage, and makes exit socially and economically
 *   catastrophic. This reading's extractiveness is high (0.68 by interval
 *   end) because the extraction — women's loss of personhood, autonomy, and
 *   property — is systematic and institutionalized, not incidental.
 *   Suppression is also high (0.72) because it depends on active enforcement
 *   by caste councils, family sanctions, and brahminical authority rejection
 *   of dissenting unions. The measurement series shows slight decay in both
 *   metrics (suppression drops from 0.78 to 0.72 over the interval),
 *   reflecting the rising challenge from reformers and constitutional
 *   democracy in the period leading to the 1955 Hindu Marriage Act.
 *
 * KEY AGENTS:
 *   - brahminical_authority_holders: Interpret texts, certify legitimacy, extract deference and material resources. Institutional power; arbitrage exit (can reinterpret texts or accept reform). Beneficiary.
 *   - patrilineal_family_heads: Control property transmission and household labor. Powerful individual-level actors; constrained exit (reformation would require dismantling property systems they depend on). Beneficiary.
 *   - married_women: Bound by sacramental obligation, unable to divorce unilaterally, lose property and social standing if they attempt exit. Powerless; identity-locked exit (self-concept fused with wife-role and caste identity). Primary payer.
 *   - caste_endogamy_enforcers: Organized bodies that punish inter-caste unions. Organized power; constrained exit (losing enforcement authority would dissolve their organizational identity). Beneficiary.
 *   - inter_caste_couples: Seek to marry across caste boundaries; face disinheritance and violence. Moderate power; trapped exit (both family break and acceptance of constraint are catastrophic). Victim.
 *   - divorced_women: Left a marriage but face permanent economic deprivation and social death under pre-1955 law. Powerless; trapped exit (even de facto separation crystallizes lifelong deprivation). Victim.
 *   - reformers and constitutional democracy advocates: Excluded from brahminical deliberation but eventually overturned this reading's legal authority through the 1955 Act. Moderate power initially; analytical position; excluded from the constraint's own operation until 1955.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(family_law_authority__hindu_dharmashastra_reading, 0.68).
domain_priors:suppression_score(family_law_authority__hindu_dharmashastra_reading, 0.72).
domain_priors:theater_ratio(family_law_authority__hindu_dharmashastra_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(family_law_authority__hindu_dharmashastra_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(family_law_authority__hindu_dharmashastra_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(family_law_authority__hindu_dharmashastra_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(family_law_authority__hindu_dharmashastra_reading, accessibility_collapse, 0.79).
narrative_ontology:constraint_metric(family_law_authority__hindu_dharmashastra_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(family_law_authority__hindu_dharmashastra_reading, tangled_rope).
narrative_ontology:human_readable(family_law_authority__hindu_dharmashastra_reading, "Hindu Dharmashastra Marriage Governance").
narrative_ontology:topic_domain(family_law_authority__hindu_dharmashastra_reading, "religious/family law/political").

domain_priors:requires_active_enforcement(family_law_authority__hindu_dharmashastra_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(family_law_authority__hindu_dharmashastra_reading, 'ae451139-a7d4-402d-b3aa-7f5542bdf5d0').
narrative_ontology:cs_kernel_codification('ae451139-a7d4-402d-b3aa-7f5542bdf5d0', fixed_text).
narrative_ontology:cs_authority_grounding('ae451139-a7d4-402d-b3aa-7f5542bdf5d0', lineage).
narrative_ontology:cs_interpretation_layer_present('ae451139-a7d4-402d-b3aa-7f5542bdf5d0').
narrative_ontology:cs_reading_relation('ae451139-a7d4-402d-b3aa-7f5542bdf5d0', family_law_authority__christian_canonical_reading, coexists_with).
narrative_ontology:cs_reading_relation('ae451139-a7d4-402d-b3aa-7f5542bdf5d0', family_law_authority__muslim_shariat_reading, coexists_with).
narrative_ontology:cs_reading_relation('ae451139-a7d4-402d-b3aa-7f5542bdf5d0', family_law_authority__parsi_zoroastrian_reading, coexists_with).
narrative_ontology:cs_reading_relation('ae451139-a7d4-402d-b3aa-7f5542bdf5d0', family_law_authority__secular_contractual_reading, influences).
narrative_ontology:cs_axiom('ae451139-a7d4-402d-b3aa-7f5542bdf5d0', foundational, vedic_sacramental_indissolubility).
narrative_ontology:cs_axiom_status(vedic_sacramental_indissolubility, overridden).
narrative_ontology:cs_axiom_grounding('ae451139-a7d4-402d-b3aa-7f5542bdf5d0', vedic_sacramental_indissolubility, deontological).
narrative_ontology:cs_axiom('ae451139-a7d4-402d-b3aa-7f5542bdf5d0', foundational, patrilineal_property_transmission_necessity).
narrative_ontology:cs_axiom_status(patrilineal_property_transmission_necessity, overridden).
narrative_ontology:cs_axiom_grounding('ae451139-a7d4-402d-b3aa-7f5542bdf5d0', patrilineal_property_transmission_necessity, empirically_contingent).
narrative_ontology:cs_axiom('ae451139-a7d4-402d-b3aa-7f5542bdf5d0', secondary, caste_hierarchy_naturalization).
narrative_ontology:cs_axiom_status(caste_hierarchy_naturalization, overridden).
narrative_ontology:cs_axiom_grounding('ae451139-a7d4-402d-b3aa-7f5542bdf5d0', caste_hierarchy_naturalization, conventional).
narrative_ontology:cs_reference_frame('ae451139-a7d4-402d-b3aa-7f5542bdf5d0', vedic_family_dharma).
narrative_ontology:cs_drift_state('ae451139-a7d4-402d-b3aa-7f5542bdf5d0', post_1947_constitutional_democracy, gap(authority_erosion, severe, true)).
narrative_ontology:cs_created_at('ae451139-a7d4-402d-b3aa-7f5542bdf5d0', '').
narrative_ontology:cs_kernel_id(family_law_authority__hindu_dharmashastra_reading, family_law_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(family_law_authority__hindu_dharmashastra_reading, brahminical_authority_holders).
narrative_ontology:constraint_beneficiary(family_law_authority__hindu_dharmashastra_reading, patrilineal_family_heads).
narrative_ontology:constraint_beneficiary(family_law_authority__hindu_dharmashastra_reading, caste_endogamy_enforcers).
narrative_ontology:constraint_victim(family_law_authority__hindu_dharmashastra_reading, married_women).
narrative_ontology:constraint_victim(family_law_authority__hindu_dharmashastra_reading, inter_caste_couples).
narrative_ontology:constraint_victim(family_law_authority__hindu_dharmashastra_reading, divorced_women).
narrative_ontology:constraint_vindicates(family_law_authority__hindu_dharmashastra_reading, vedic_ritual_supremacy).
narrative_ontology:constraint_vindicates(family_law_authority__hindu_dharmashastra_reading, caste_hierarchy_naturalization).
narrative_ontology:constraint_vindicates(family_law_authority__hindu_dharmashastra_reading, patrilineal_property_transmission).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interpret and enforce dharmashastra texts (Manusmriti, Yajnavalkya) governing marriage. Control legitimacy of unions through ritual certification and caste verification. Collect deference, social standing, and (historically) material resources from families seeking brahminical sanction. Their authority is grounded in textual mastery and claim to vedic lineage transmission.
narrative_ontology:constraint_stakeholder(family_law_authority__hindu_dharmashastra_reading, brahminical_authority_holders, agenda_setter,
    institutional, civilizational, arbitrage, regional).

% Benefit from marriage rules that transmit property through male lineage, ensure wife's labor and reproductive capacity flow to the household, and bind wives through sacramental indissolubility. A wife cannot easily exit the marriage or claim independent property; her children legally belong to the husband's patriline. The constraint preserves family patrimony and male control over accumulation.
narrative_ontology:constraint_stakeholder(family_law_authority__hindu_dharmashastra_reading, patrilineal_family_heads, beneficiary,
    powerful, generational, constrained, regional).

% Bound by sacramental indissolubility (pativrata dharma) that makes the marriage a permanent ritual obligation. Cannot divorce unilaterally; leaving the marriage violates dharmic duty and invokes caste and family sanctions (separation = ritual pollution, loss of social standing). Legally and ritually, their personhood is constituted through the marriage relationship — exit would require dismantling their identity within the community.
narrative_ontology:constraint_stakeholder(family_law_authority__hindu_dharmashastra_reading, married_women, payer,
    powerless, biographical, identity_locked, regional).

% Caste councils and kin networks that enforce endogamy (marrying within caste) and punish inter-caste unions through social ostracism, property confiscation, and violence. The constraint's endogamy requirement preserves caste hierarchy and their enforcement power. They benefit from the authority to adjudicate legitimacy and the deference extracted from families seeking caste approval.
narrative_ontology:constraint_stakeholder(family_law_authority__hindu_dharmashastra_reading, caste_endogamy_enforcers, beneficiary,
    organized, generational, constrained, regional).

% Seeking to marry across caste lines face caste council rejection, family disinheritance, and community violence. The constraint makes inter-caste marriage ceremonially illegitimate under dharmashastra and socially catastrophic. Their only options are submission to caste authority or accepting complete family rupture and relocation; neither amounts to genuine exit.
narrative_ontology:constraint_stakeholder(family_law_authority__hindu_dharmashastra_reading, inter_caste_couples, payer,
    moderate, biographical, trapped, regional).

% In pre-1955 dharmashastra law, a woman who left a marriage (whether by divorce or abandonment) is ritually impure, loses all claim to the joint family property, loses guardianship of children, and faces permanent social stigma ('widow' status even if the husband lives). Remarriage is forbidden. They are trapped in the marriage even after de facto separation because exit crystallizes lifelong economic deprivation and social death.
narrative_ontology:constraint_stakeholder(family_law_authority__hindu_dharmashastra_reading, divorced_women, payer,
    powerless, biographical, trapped, regional).

% Applied and codified dharmashastra law in courts (late 1800s–1955), narrating it as stable, ancient, and customary. Created the appearance of doctrinal consistency and textual authority while selectively emphasizing patrilineal and caste-endogamy provisions that aligned with British administrative interests. Their adjudication legitimated brahminical interpretations and froze them into legal doctrine.
narrative_ontology:constraint_stakeholder(family_law_authority__hindu_dharmashastra_reading, colonial_british_judges, observer,
    institutional, biographical, analytical, regional).

% Argued from the 1940s onward that dharmashastra marriage governance violated constitutional equality and human rights. Advocated for secular family law granting women divorce rights, property rights, and inter-caste marriage recognition. Their voice was structurally excluded from brahminical authority circles (who dismissed reform as anti-vedic) but eventually prevailed in the 1955 Hindu Marriage Act, which formally ended this reading's legal status.
narrative_ontology:constraint_stakeholder(family_law_authority__hindu_dharmashastra_reading, independent_indian_reformers, excluded,
    moderate, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(family_law_authority__hindu_dharmashastra_reading, brahminical_authority_holders).
narrative_ontology:fixing_cost_class(family_law_authority__hindu_dharmashastra_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the coordination problem of property transmission through patrilineal descent and ensures stable household composition for agricultural production and ritual observance by embedding marriage in sacramental, indissoluble obligation. Ritual certification by brahminical authority provides a unified legitimacy standard across regions.
% TRANSFER_FUNCTION: Moves a woman's labor, reproductive capacity, property rights, and social autonomy from her natal family to the groom's patrilineal household at marriage. The constraint ensures this transfer is permanent and irreversible through sacramental binding; wives' exits are blocked by ritual obligation, property claims are dissolved, and social personhood is forfeit.
% ABSENT_VOICES: Women trapped in abusive or infertile marriages and inter-caste couples seeking union have no voice in brahminical or family deliberation — the constraint was authored by and for male lineage heads and brahminical authorities. Reform voices from educated women and secular nationalists were excluded during the entire period this reading governed actual law (until 1955). Their absence was not accidental — the constraint depended on silencing objections from the most-extracted seats.
% DISAPPEARANCE_RATIONALE: If this constraint (sacramental indissolubility, caste endogamy enforcement, wife's non-personhood in property/divorce) vanished overnight, women would immediately claim divorce rights, inter-caste marriages would proliferate, wives would claim joint property, and the patrilineal property transmission system would face fundamental reorganization. The 1955 Hindu Marriage Act and subsequent amendments demonstrate exactly this rearrangement: women gained divorce rights, inter-caste marriage became legal, and joint family property devolved to wives. The constraint's disappearance was the signal event that reorganized the legal family structure.
% FOUNDING_PROBLEM: Pre-agricultural-surplus India: need to formalize lineage for property inheritance, stabilize household composition for ritual observance, prevent widow-remarriage that would create competing claims to patrimonial property, and establish unified legitimacy standards across kinship networks.
% FOUNDING_PROBLEM_CORROBORATION: The brahminical tradition attests the founding problem was real for agrarian household management. Economic historians (Sharma, Guha) confirm that textile-production and agricultural societies required stable patrilineal property transmission and that sacramental marriage served this function in pre-1800 contexts. However, independent reformers, women's rights advocates, and the Constitution's framers attested that by 1950, the founding problem had been superseded: industrial capitalism decoupled property transmission from household ritual stability, and constitutional democracy made indissolubility and caste hierarchy incompatible with citizenship rights. The founding problem status is attested as dead by voices outside the brahminical beneficiary circle.
narrative_ontology:disappearance_verdict(family_law_authority__hindu_dharmashastra_reading, world_rearranges).
narrative_ontology:founding_problem_status(family_law_authority__hindu_dharmashastra_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(family_law_authority__hindu_dharmashastra_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(family_law_authority__hindu_dharmashastra_reading, 'none', 1).
narrative_ontology:epsilon_provenance(family_law_authority__hindu_dharmashastra_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(family_law_authority__hindu_dharmashastra_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(family_law_authority__hindu_dharmashastra_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(family_law_authority__hindu_dharmashastra_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is high because the constraint systematically transfers women's labor, reproductive capacity, property, and autonomy to the patrilineal household and denies them the exit path (divorce). The transfer is not incidental to any real coordination function — it is the point. Suppression is even higher because the extraction depends entirely on active enforcement: caste councils rejecting inter-caste couples, families disowning women who leave, brahminical authority refusing to certify 'illegitimate' unions, and colonial courts codifying these refusals into binding precedent. The theater ratio (0.41 at interval end, stable throughout) indicates that about 40% of the enforcement activity is theatrical: brahminical recitation of vedic texts defending the constraint's naturalness, ritual performance of the samskara ceremony itself (which serves to bind the couple through sacred obligation), and framing the constraint as inevitable cosmic order rather than choice. The remaining 60% is functional enforcement — actual property transfer, child custody assignment, social punishment. The measurement series is flat across the interval because this reading's structural operation did not change substantively during its legal reign (1800–1955); the decay in suppression_requirement reflects only the rising external challenge from reformers, not internal transformation. At the 1955 threshold, this reading's legal authority ceased abruptly — a step discontinuity, not a gradual erosion.
 *
 * PERSPECTIVAL GAP:
 *   The agenda_setter seat (brahminical authority) and the payer seats (married women, divorced women, inter-caste couples) should compute radically different types. From the brahminical seat, this is a rope: real coordination function (property transmission, household stability, ritual completeness), voluntary participation (marriage is a choice to enter the samskara), and shared benefit (women gain the social status of wife, brahminical authority gains deference, families gain property stability). From the payer seats, this is a snare: the coordination function exists but the extraction is the point, exit is suppressed by violence and social death, and the constraint's persistence depends entirely on coercion. The engine computes this divergence from the structural data — married_women's identity_locked exit and powerless position vs. brahminical_authority_holders' institutional power and arbitrage exit. The claim of rope reflects the agenda_setter's framing; the metrics reflect the payer seats' actual experience.
 *
 * DIRECTIONALITY LOGIC:
 *   Married women: full target (d near 1.0). Powerless position, identity-locked exit (self-concept fused with wife-role; leaving would require rejecting caste, family, and religious identity simultaneously), high structural extraction (loss of all property and social standing on exit), and trapped within the household by ritual obligation and enforcement machinery. Inter-caste couples: high target (d > 0.8). Moderate power but trapped exit — both family disinheritance and acceptance of the constraint are catastrophic. Divorced women: full target (d ≈ 1.0). Powerless, trapped exit, permanent deprivation. Patrilineal family heads: beneficiary (d < 0.3). Powerful position, property flows toward them, constrained exit (would require dismantling inheritance systems) but benefits exceed costs so exit is unlikely. Brahminical authority holders: beneficiary (d ≈ 0.15). Institutional power, arbitrage exit (can reinterpret texts, negotiate with reformers, adapt to new regimes), deference and authority collected without material cost. Caste endogamy enforcers: beneficiary (d < 0.4). Organized power, constrained exit, material and social status benefits from enforcement role. The directionality derivation follows from beneficiary/victim declarations and exit modulation: victims with trapped or identity-locked exit get higher d; beneficiaries with arbitrage or powerful-position exit get lower d.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading exhibits clear mandatrophy (mandate outliving function). The founding problem — stable patrilineal property transmission and ritual household composition for agrarian production — is dead by 1950: industrial capitalism decoupled household property from ritual stability, and constitutional democracy made indissolubility and caste hierarchy incompatible with citizenship. The constraint persists from 1900–1955 with rising suppression as reformers mount challenges, but the suppression is pure enforcement (caste violence, family ostracism) with no underlying coordination function justifying it. The measurement series shows near-zero decay in base_extractiveness while suppression drops slightly — the constraint is maintained through sheer institutional and family will, not because it solves any ongoing coordination problem. The 1955 Hindu Marriage Act is the mandate-death verdict: the Indian state formally declared the founding problem obsolete and the constraint structurally incompatible with constitutional rights. The constraint does not vanish — brahminical readings persist and caste endogamy norms continue informally — but its legal authority dies, and with it the institutional mandate. This is the classic mandatrophy arc: coordination function becomes vestigial or hostile; constraint persists through enforcement inertia and beneficiary power; mandate is eventually formally revoked. The constraint should be reclassified as piton post-1955 (persists through inertia and theatrical maintenance), but from the dharmashastra reading's own standpoint within its period of legal authority (1800–1955), it is tangled rope: genuine coordination function + asymmetric extraction + active enforcement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sacramental_vs_contractual_framing,
    'Is marriage constitutively a sacrament (samskara) binding through ritual obligation, or is it a civil contract revocable by will?',
    'The dharmashastra reading takes it as sacrament; the secular_contractual reading takes it as contract. No empirical data resolves the question — it is a choice of interpretive frame. However, the constitutional courts'' choice (1955 onward) imposed the contractual framing as law, effectively foreclosing the sacramental reading within India''s legal system.',
    'If sacrament: indissolubility is justified by cosmic/dharmic necessity, and exit represents ritual pollution and dharmic failure (supports high suppression and identity-lock). If contract: indissolubility cannot be justified without consent, and exit is a legal right (supports low suppression and free exit). The framing choice determines whether women''s inability to exit is a natural law or an extractive coercion.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sacramental_vs_contractual_framing, conceptual, 'Whether marriage''s authority grounds in ritual sacrament or civil contract — fundamentally different kernel readings, not measurable differences.').

omega_variable(
    foundational_problem_status_contestation,
    'Has the founding problem (property transmission, household stability) actually been solved, or does it persist?',
    'Reformers and constitutional framers attested in 1950 that the founding problem was dead (industrial capitalism and state administration solved property and household functions). Brahminical authorities attested it was still live (ritual and caste-based family formation remained necessary for social order). Post-1955 empirical observation: inter-caste marriages proliferate, wives claim property, divorce rates rise, families restructure around conjugal units rather than patrilines — all without societal collapse. The verdict is empirically favorable to the reform position (founding problem is dead), but lived experience in conservative communities still reflects the constraint''s grip.',
    'If the founding problem is dead, mandatrophy is clear and the constraint should be reclassified as piton or snare post-1955 (persisting through inertia and beneficiary power, not function). If the founding problem is still live, the constraint retains a coordination justification and tangled_rope classification stands. The empirical evidence favors the ''dead'' verdict, but some communities continue to experience the constraint as functional (conservative joint families, caste enforcement networks).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(foundational_problem_status_contestation, empirical, 'Whether the founding problem remains live or has been superseded by institutional and economic change.').

omega_variable(
    identity_lock_mechanism_internalization,
    'Is married women''s identity-lock structural (legal disabilities + property loss + family disinheritance) or internalized (beliefs in pativrata dharma and ritual purity)?',
    'Pre-1955: both structural and internalized — legal disability was enforced, and women internalized the dharmic obligation as duty (pativrata). Post-1955: structural disabilities are removed by law, but many women (especially in conservative families) continue to experience the constraint as binding through internalized obligation to caste honor, family approval, and religious duty. Measuring exit choices pre- and post-1955 would show the degree to which structural removal translates to behavioral exit.',
    'If largely structural, removing legal disabilities should produce rapid exit (which it does — divorce rates rise post-1955, inter-caste marriages become visible). If largely internalized, legal removal is insufficient — women carry the suppression with them even after de jure exit. Likely answer: both operate, with the balance varying by community, education, and exposure to alternative framings. Conservative communities show continued identity-lock despite legal change; more cosmopolitan/educated women exit more readily.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism_internalization, empirical, 'Whether identity-lock suppression is structural or internalized — affects post-exit trajectories and the reach of legal reform.').

omega_variable(
    brahminical_textual_authority_grounding,
    'Do dharmashastra texts (Manusmriti, Yajnavalkya) represent genuinely ancient, unified dharmic law, or are they post-colonial selective codifications reflecting brahminical interests and colonial administrative choices?',
    'Textual scholarship (Pollock, Lariviere) documents that manusmriti itself is a composite text with multiple layers and interpretations; pre-colonial application was highly variable across regions and castes; colonial judges selected interpretations that aligned with both brahminical authority and British administrative hierarchy (patrilineal property, caste stability). The texts are real and ancient, but their ''unified dharmic law'' is partly brahminical construction and partly colonial artifact.',
    'If the texts represent genuinely unified ancient dharma, the constraint has deep civilizational warrant and authority_grounding=lineage is accurate. If the texts are brahminically constructed and colonially reified, the authority is more recent and more contingent than brahminical claims assert, and authority_grounding might be better characterized as extraction (authorities benefit from text-based legitimacy they have selectively authored). This omega documents the kernel''s authority uncertainty.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(brahminical_textual_authority_grounding, conceptual, 'Whether dharmashastra represents deep textual lineage or partially brahminical/colonial construction — affects the legitimacy grounding of the reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(family_law_authority__hindu_dharmashastra_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fami_tr_t0, family_law_authority__hindu_dharmashastra_reading, theater_ratio, 0, 0.38).
narrative_ontology:measurement(fami_tr_t3, family_law_authority__hindu_dharmashastra_reading, theater_ratio, 3, 0.39).
narrative_ontology:measurement(fami_tr_t6, family_law_authority__hindu_dharmashastra_reading, theater_ratio, 6, 0.4).
narrative_ontology:measurement(fami_tr_t12, family_law_authority__hindu_dharmashastra_reading, theater_ratio, 12, 0.41).
narrative_ontology:measurement(fami_tr_t18, family_law_authority__hindu_dharmashastra_reading, theater_ratio, 18, 0.41).
narrative_ontology:measurement(fami_tr_t25, family_law_authority__hindu_dharmashastra_reading, theater_ratio, 25, 0.41).

% Extraction over time
narrative_ontology:measurement(fami_be_t0, family_law_authority__hindu_dharmashastra_reading, base_extractiveness, 0, 0.71).
narrative_ontology:measurement(fami_be_t3, family_law_authority__hindu_dharmashastra_reading, base_extractiveness, 3, 0.7).
narrative_ontology:measurement(fami_be_t6, family_law_authority__hindu_dharmashastra_reading, base_extractiveness, 6, 0.7).
narrative_ontology:measurement(fami_be_t12, family_law_authority__hindu_dharmashastra_reading, base_extractiveness, 12, 0.69).
narrative_ontology:measurement(fami_be_t18, family_law_authority__hindu_dharmashastra_reading, base_extractiveness, 18, 0.68).
narrative_ontology:measurement(fami_be_t25, family_law_authority__hindu_dharmashastra_reading, base_extractiveness, 25, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(fami_su_t0, family_law_authority__hindu_dharmashastra_reading, suppression_requirement, 0, 0.78).
narrative_ontology:measurement(fami_su_t3, family_law_authority__hindu_dharmashastra_reading, suppression_requirement, 3, 0.77).
narrative_ontology:measurement(fami_su_t6, family_law_authority__hindu_dharmashastra_reading, suppression_requirement, 6, 0.76).
narrative_ontology:measurement(fami_su_t12, family_law_authority__hindu_dharmashastra_reading, suppression_requirement, 12, 0.74).
narrative_ontology:measurement(fami_su_t18, family_law_authority__hindu_dharmashastra_reading, suppression_requirement, 18, 0.72).
narrative_ontology:measurement(fami_su_t25, family_law_authority__hindu_dharmashastra_reading, suppression_requirement, 25, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(family_law_authority__hindu_dharmashastra_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(family_law_authority__hindu_dharmashastra_reading, 0.12).
narrative_ontology:affects_constraint(family_law_authority__hindu_dharmashastra_reading, family_law_authority__muslim_shariat_reading).
narrative_ontology:affects_constraint(family_law_authority__hindu_dharmashastra_reading, family_law_authority__christian_canonical_reading).
narrative_ontology:affects_constraint(family_law_authority__hindu_dharmashastra_reading, family_law_authority__secular_contractual_reading).
narrative_ontology:affects_constraint(family_law_authority__hindu_dharmashastra_reading, family_law_authority__parsi_zoroastrian_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of five readings of the kernel 'family law authority' — a contested kernel in post-1947 India governing which legal tradition (dharmashastra, shariat, Christian canon, Zoroastrian law, or secular state law) sets marriage rules. Each reading has its own ε, beneficiary/victim structure, and chief omega uncertainties. All five readings coexist institutionally and politically; the 1955 Hindu Marriage Act formally privileged the secular_contractual_reading for Hindu-professing Indians while preserving shariat courts for Muslims and Christian personal law for Christians, creating a plural legal regime. The dharmashastra reading's legal authority is superseded post-1955 but persists in informal enforcement (caste councils, family pressure). The sibling readings' constraints are linked via network.affects_constraints because they compete for legitimacy and institutional authority — a change in one reading's institutional standing affects the others' pressure to maintain their own authority.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(family_law_authority__hindu_dharmashastra_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
