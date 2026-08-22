% ============================================================================
% CONSTRAINT STORY: family_law_authority__parsi_zoroastrian_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_family_law_authority__parsi_zoroastrian_reading, []).

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
 *   constraint_id: family_law_authority__parsi_zoroastrian_reading
 *   human_readable: Parsi Zoroastrian Endogamous Marriage Regime
 *   domain: comparative_law/political_theory/religious_governance
 *
 * SUMMARY:
 *   The Parsi community of India — roughly 57,000 people by the 2011 census,
 *   down from about 114,000 in 1941 — governs marriage through Zoroastrian
 *   religious law as codified in the Parsi Marriage and Divorce Act, 1936: a
 *   marriage is ritually valid only when solemnized by a hereditary priest
 *   (mobed), and the community's institutions treat marriage outside the
 *   group as forfeiting standing, with the consequence falling asymmetrically
 *   on women (the children of Parsi women who marry out are not accepted as
 *   Parsis, while the children of Parsi men who marry out are). The regime is
 *   defended as the wall of the vessel: Zoroastrianism accepts no converts,
 *   so the community is the religion's only carrier, and marriage rules are
 *   its reproduction machinery. It is contested from inside by reformists, by
 *   litigating women (culminating in the Supreme Court's 2017 Goolrukh Gupta
 *   ruling upholding the exclusion), and by a liberalizing diaspora. Per the
 *   epsilon-invariance principle this story is ONE reading of the
 *   family_law_authority kernel: it authors epsilon for the standing
 *   endogamy-enforcing arrangement as the Zoroastrian reading itself assesses
 *   it — a reading that concedes heavy costs on identifiable seats while
 *   holding the arrangement necessary. The claim and the metrics are
 *   independent authored facts: the claimed type is what I believe
 *   structurally true of this arrangement; the metrics are what I believe
 *   descriptively true of its operation; the engine computes per-seat
 *   classifications and any divergence from the claim is the datum.
 *
 * KEY AGENTS:
 *   - - mobed_priesthood: Primary agenda-setter (organized/identity_locked) — holds the ritual-validity gate; collects ceremonial fees, veto power, and centrality from the arrangement it enforces
 *   - - punchayat_trusteeship: Secondary agenda-setter and beneficiary (institutional/constrained) — adjudicates community status and gates trust housing and doles on recognized standing
 *   - - parsi_women_out_marriers: Primary target (moderate/identity_locked) — bears the sharpest extraction: their children are denied recognition
 *   - - children_of_out_married_women: Pure target (powerless/trapped) — excluded by a rule they had no hand in making and no forum to contest
 *   - - parsi_youth_intermarriage_pool: Target with residual subsidy (moderate/constrained) — pays in restricted choice, receives full standing if compliant
 *   - - non_parsi_spouses: Excluded party (moderate/constrained) — barred from conversion and ritual recognition, with no seat in the forums that rule on them
 *   - - diaspora_zoroastrian_associations: Incidental beneficiary under schism cost (organized/mobile) — inherits the identity framework while escaping the enforcement's reach
 *   - - indian_high_courts_supreme_court: Analytical observer (institutional/analytical) — ratifies or unsettles the regime without administering it day to day
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(family_law_authority__parsi_zoroastrian_reading, 0.68).
domain_priors:suppression_score(family_law_authority__parsi_zoroastrian_reading, 0.64).
domain_priors:theater_ratio(family_law_authority__parsi_zoroastrian_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(family_law_authority__parsi_zoroastrian_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(family_law_authority__parsi_zoroastrian_reading, suppression_requirement, 0.64).
narrative_ontology:constraint_metric(family_law_authority__parsi_zoroastrian_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(family_law_authority__parsi_zoroastrian_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(family_law_authority__parsi_zoroastrian_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(family_law_authority__parsi_zoroastrian_reading, tangled_rope).
narrative_ontology:human_readable(family_law_authority__parsi_zoroastrian_reading, "Parsi Zoroastrian Endogamous Marriage Regime").
narrative_ontology:topic_domain(family_law_authority__parsi_zoroastrian_reading, "comparative_law/political_theory/religious_governance").

domain_priors:requires_active_enforcement(family_law_authority__parsi_zoroastrian_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(family_law_authority__parsi_zoroastrian_reading, 'c56ffc70-1cf6-481f-8b93-5948adf6d68d').
narrative_ontology:cs_kernel_codification('c56ffc70-1cf6-481f-8b93-5948adf6d68d', fixed_text).
narrative_ontology:cs_authority_grounding('c56ffc70-1cf6-481f-8b93-5948adf6d68d', lineage).
narrative_ontology:cs_interpretation_layer_present('c56ffc70-1cf6-481f-8b93-5948adf6d68d').
narrative_ontology:cs_reading_relation('c56ffc70-1cf6-481f-8b93-5948adf6d68d', family_law_authority__hindu_dharmashastra_reading, coexists_with).
narrative_ontology:cs_reading_relation('c56ffc70-1cf6-481f-8b93-5948adf6d68d', family_law_authority__muslim_shariat_reading, coexists_with).
narrative_ontology:cs_reading_relation('c56ffc70-1cf6-481f-8b93-5948adf6d68d', family_law_authority__christian_canonical_reading, coexists_with).
narrative_ontology:cs_reading_relation('c56ffc70-1cf6-481f-8b93-5948adf6d68d', family_law_authority__secular_contractual_reading, coexists_with).
narrative_ontology:cs_axiom('c56ffc70-1cf6-481f-8b93-5948adf6d68d', foundational, community_is_the_faiths_only_carrier).
narrative_ontology:cs_axiom_status(community_is_the_faiths_only_carrier, holdable).
narrative_ontology:cs_axiom_grounding('c56ffc70-1cf6-481f-8b93-5948adf6d68d', community_is_the_faiths_only_carrier, deontological).
narrative_ontology:cs_axiom('c56ffc70-1cf6-481f-8b93-5948adf6d68d', foundational, endogamy_required_for_transmission).
narrative_ontology:cs_axiom_status(endogamy_required_for_transmission, holdable).
narrative_ontology:cs_axiom_grounding('c56ffc70-1cf6-481f-8b93-5948adf6d68d', endogamy_required_for_transmission, empirically_contingent).
narrative_ontology:cs_reference_frame('c56ffc70-1cf6-481f-8b93-5948adf6d68d', vendidad_endogamous_community_carrier).
narrative_ontology:cs_drift_state('c56ffc70-1cf6-481f-8b93-5948adf6d68d', contemporary_post_goolrukh_ruling, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('c56ffc70-1cf6-481f-8b93-5948adf6d68d', '').
narrative_ontology:cs_kernel_id(family_law_authority__parsi_zoroastrian_reading, family_law_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(family_law_authority__parsi_zoroastrian_reading, mobed_priesthood).
narrative_ontology:constraint_beneficiary(family_law_authority__parsi_zoroastrian_reading, punchayat_trusteeship).
narrative_ontology:constraint_victim(family_law_authority__parsi_zoroastrian_reading, parsi_women_out_marriers).
narrative_ontology:constraint_victim(family_law_authority__parsi_zoroastrian_reading, children_of_out_married_women).
narrative_ontology:constraint_victim(family_law_authority__parsi_zoroastrian_reading, parsi_youth_intermarriage_pool).
narrative_ontology:constraint_victim(family_law_authority__parsi_zoroastrian_reading, non_parsi_spouses).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(family_law_authority__parsi_zoroastrian_reading, parsi_youth_intermarriage_pool).
narrative_ontology:constraint_beneficiary(family_law_authority__parsi_zoroastrian_reading, diaspora_zoroastrian_associations).
narrative_ontology:constraint_victim(family_law_authority__parsi_zoroastrian_reading, diaspora_zoroastrian_associations).
narrative_ontology:constraint_vindicates(family_law_authority__parsi_zoroastrian_reading, hereditary_descent_transmission_doctrine).
narrative_ontology:constraint_vindicates(family_law_authority__parsi_zoroastrian_reading, community_as_sole_religious_vessel).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hereditary priestly class (mobeds) who alone may solemnize the ashirvad marriage ceremony; a marriage without their officiation lacks ritual validity, and they refuse to perform rites joining a Parsi to a non-Parsi. They collect ceremonial fees and occupy the community's ritual center; their refusal power is the regime's enforcement core. Leaving the office means abandoning a hereditary vocation and a standing that has no life outside the community.
narrative_ontology:constraint_stakeholder(family_law_authority__parsi_zoroastrian_reading, mobed_priesthood, agenda_setter,
    organized, generational, identity_locked, regional).

% Elected trustees of community bodies such as the Bombay Parsi Punchayet who adjudicate disputes over community status, administer housing colonies, charitable trusts, and doles whose eligibility turns on recognized Parsi standing, and issue public resolutions on intermarriage. They perform no rituals but decide whose status counts; the regime's definitions determine who may approach the assets they control.
narrative_ontology:constraint_stakeholder(family_law_authority__parsi_zoroastrian_reading, punchayat_trusteeship, agenda_setter,
    institutional, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(family_law_authority__parsi_zoroastrian_reading, punchayat_trusteeship, beneficiary).

% Parsi women who marry non-Parsi men. Under the prevailing rule their children are not accepted as Parsis, while the children of Parsi men who marry out are; several have litigated for recognition — leading to the 2017 Supreme Court decision upholding their exclusion — and continue organizing against the asymmetry. Raised inside the community's institutions, they describe departure as losing who they are, yet they stay and contest rather than walk away.
narrative_ontology:constraint_stakeholder(family_law_authority__parsi_zoroastrian_reading, parsi_women_out_marriers, payer,
    moderate, biographical, identity_locked, regional).

% Children of Parsi mothers and non-Parsi fathers, barred from navjote initiation and community recognition by the maternal-descent bar. They chose nothing and can elect into nothing; their standing depends entirely on rules made in forums they cannot enter. Some participate informally in community life; formally they are outside.
narrative_ontology:constraint_stakeholder(family_law_authority__parsi_zoroastrian_reading, children_of_out_married_women, payer,
    powerless, biographical, trapped, regional).

% Young adult Parsis of both sexes facing a shrinking in-group marriage market and rising rates of out-marriage. Those who marry in receive full standing, trust access, and ritual belonging; those who marry out absorb sanctions that fall hardest on the women. Emigration to diaspora communities with softer practice offers partial relief, but most remain inside the sanction's reach.
narrative_ontology:constraint_stakeholder(family_law_authority__parsi_zoroastrian_reading, parsi_youth_intermarriage_pool, payer,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(family_law_authority__parsi_zoroastrian_reading, parsi_youth_intermarriage_pool, beneficiary).

% Non-Parsi husbands and wives of Parsis. Zoroastrianism accepts no conversion, so no rite admits them; their marriages are civilly valid but ritually unrecognized, and the children of Parsi wives are barred from recognition. They hold no seat in the panchayats, priestly bodies, or litigation that define their family's standing; their recourse is private pressure and public commentary.
narrative_ontology:constraint_stakeholder(family_law_authority__parsi_zoroastrian_reading, non_parsi_spouses, payer,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(family_law_authority__parsi_zoroastrian_reading, non_parsi_spouses, excluded).

% Zoroastrian associations in North America, Britain, and elsewhere that inherit the same scriptural framework and ritual forms but increasingly accept intermarried families and the children of Parsi women. They draw identity continuity from the tradition while bearing the cost of collision with Indian-centered orthodoxy that claims definitional authority; distance from the sanctioning centers softens enforcement on them.
narrative_ontology:constraint_stakeholder(family_law_authority__parsi_zoroastrian_reading, diaspora_zoroastrian_associations, beneficiary,
    organized, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(family_law_authority__parsi_zoroastrian_reading, diaspora_zoroastrian_associations, payer).

% The judiciary applying the Parsi Marriage and Divorce Act, 1936 and deciding status disputes. In Goolrukh Gupta v. Burjor Pardiwala (2017) the Supreme Court held that a Parsi woman marrying outside presumptively loses her religious identity, ratifying the exclusion; the courts ratify or unsettle the regime but do not administer it day to day.
narrative_ontology:constraint_stakeholder(family_law_authority__parsi_zoroastrian_reading, indian_high_courts_supreme_court, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(family_law_authority__parsi_zoroastrian_reading, mobed_priesthood).
narrative_ontology:fixing_cost_class(family_law_authority__parsi_zoroastrian_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a bounded, self-reproducing membership for a micro-minority that accepts no converts: it defines who counts as Parsi, channels marriage inside the group, and hands ritual, linguistic, and institutional continuity from one generation to the next under heavy assimilation pressure from the surrounding society.
% TRANSFER_FUNCTION: Moves marital choice and lineage recognition from individual members to the collective boundary: standing, ritual services, and eligibility for trust housing and doles flow only to those who marry in-group, and the cost of the boundary falls disproportionately on women who marry out and on their children, whose recognition is transferred away to preserve the descent line.
% ABSENT_VOICES: Non-Parsi spouses and the children of out-married Parsi women are ruled by bodies they cannot enter; reformist members speak in community publications but are outvoted in trustee elections; diaspora associations that already accept intermarried families are discounted by the Indian orthodoxy that claims definitional authority.
% DISAPPEARANCE_RATIONALE: If the endogamy regime and its enforcement vanished overnight, out-marriage would normalize within a generation as it already has in the diaspora, the recognized definition of Parsi would widen or dissolve into voluntary affiliation, trust eligibility rules would be rewritten, and the priesthood's gatekeeping centrality would collapse; the community would reorganize around chosen rather than policed belonging.
% FOUNDING_PROBLEM: Survival of a tiny religious minority that accepts no converts and carries its faith only through hereditary descent, surrounded by numerically dominant traditions and later by open secular society: the community is the religion's only vessel, so the marriage rules were built as the vessel's wall.
% FOUNDING_PROBLEM_CORROBORATION: Indian census series record the Parsi population falling from about 114,000 in 1941 to about 57,000 in 2011, and independent demographers attest the decline the founding problem names. What no source outside the beneficiary set attests is that this arrangement answers it: reformist demographers and community physicians argue endogamy shrinks the marriage pool and accelerates the very decline it is meant to prevent, and the thriving liberalized diaspora is cited against the necessity claim.
narrative_ontology:disappearance_verdict(family_law_authority__parsi_zoroastrian_reading, world_rearranges).
narrative_ontology:founding_problem_status(family_law_authority__parsi_zoroastrian_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(family_law_authority__parsi_zoroastrian_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(family_law_authority__parsi_zoroastrian_reading, 'none', 1).
narrative_ontology:epsilon_provenance(family_law_authority__parsi_zoroastrian_reading, 0.68, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(family_law_authority__parsi_zoroastrian_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(family_law_authority__parsi_zoroastrian_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(family_law_authority__parsi_zoroastrian_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is 0.68 because the regime's costs are concentrated, not diffuse: marital autonomy and lineage recognition are withdrawn from identifiable seats (women who marry out lose recognition for their children; youth face a shrinking in-group pool), while the reading itself concedes these costs are real — it prices them as necessary. Suppression is 0.64 as a RAW STRUCTURAL property, unscaled by power or scope: the sanction stack (refusal of rites, loss of fire temple and funerary standing, trust ineligibility, family rupture) is heavy but bounded — civil marriage remains lawful and no coercive force prevents departure. Theater ratio is 0.28: the ritual and boundary functions are genuine, but a growing share of activity is performative — demographic-alarm rhetoric and purity discourse deployed to justify hardening beyond functional need, especially after 2017. Accessibility collapse is 0.55: alternatives (civil marriage under the Special Marriage Act, diaspora affiliation, quiet departure) survive once the constraint is understood; they do not collapse as a natural law's would. Resistance is 0.58: sustained internal reformism, litigation, and rising out-marriage defy the sanction stack. The temporal series run on ONE shared grid (points 0-90 at 15-year steps, 1936-2026): extractiveness climbs as voluntary compliance erodes and enforcement bites against rising demand; suppression_requirement rises in step because the story specifically tracks enforcement intensification — panchayat hardening, the 2017 ratification, social-media-era boundary policing — not merely shifting extraction. Boltzmann: identity_coordination with the type-default floor (0.08, no override). The FNL gaming risk is live here and flagged deliberately: the identity framing is genuine (a micro-minority with no conversion mechanism has a real boundary-maintenance problem), but the gender-asymmetric concentration of cost on the least powerful seats is exactly the coupling pattern the complexity offset must not excuse.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seats the arrangement is the community's load-bearing wall: the priesthood experiences its gatekeeping as sacred office, the trusteeship as fiduciary duty over assets that would mean nothing without a bounded community. From the payer seats the same structure operates as enforced forfeiture — a woman who marries out does not experience the loss of her children's recognition as coordination cost. The diaspora seat computes a third thing: the framework as inheritance, the enforcement as distant and optional. The engine computes this per-seat divergence from the structural data (power, exit, role); the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   The priesthood and trusteeship sit near the beneficiary end: they collect authority, fees, and asset-gating power, and their exits are poor (identity_locked office; constrained trusteeship), which anchors rather than damps their subsidy. Women out-marriers and their children sit near the full-target end — trapped or identity_locked exit amplifies effective extraction toward the maximum for their seats. The youth pool sits mid-high: it pays in restricted choice but receives standing, ritual belonging, and trust access if it complies, damping d below the pure-target seats. Non-Parsi spouses are targets with mobile exit — they are already outside the community, so the sanction reaches them through spouse and children rather than through withheld standing, damping chi relative to trapped seats. Diaspora associations derive low d from incidental benefit plus mobility, pulled upward by schism cost. The judiciary is analytical: it observes and ratifies without collecting or paying.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is live and externally corroborated (census-documented decline), so the mismatch consumer sees status=live paired with verdict=world_rearranges — no zombie flag fires on the genealogy interview. But the endogamy_survival_efficacy omega tracks the subtler failure: if liberalized diaspora communities thrive demographically while orthodox centers shrink, the arrangement's means die while its mandate persists, and the constraint drifts piton-ward — theatrical boundary maintenance over a dissolving base, with the theater_ratio series already climbing. The tangled_rope classification is what prevents mislabeling in both directions: calling this a snare would erase the genuine survival function that a no-conversion micro-minority actually has; calling it a rope would erase the gendered, concentrated extraction that the payer seats demonstrably bear. The hybrid category holds both faces in view and lets the temporal data say which face is winning.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_instantiation,
    'This constraint is one reading of the family_law_authority kernel — marriage as community-preserving institution under Zoroastrian religious law. What would the sibling readings (hindu_dharmashastra_reading, muslim_shariat_reading, christian_canonical_reading, secular_contractual_reading) change structurally?',
    'Generate and compare the sibling stories: victim sets, transfer directions, and epsilon values are authored independently per reading; divergence in computed classification across the family locates what the kernel contest actually turns on.',
    'Under the secular_contractual_reading the victim set empties (autonomous consent suffices) and epsilon collapses toward zero; under the shariat reading the transfer runs through contractual terms negotiable at inception rather than through descent-barred status; the classification of this story is valid only for this reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_instantiation, conceptual, 'Committer structure: one reading of a five-reading kernel; siblings are separate constraints, not hedges inside this one.').

omega_variable(
    gender_asymmetry_doctrinal_basis,
    'Is the bar on recognizing the children of out-married Parsi women (while the children of out-married Parsi men are accepted) grounded in Zoroastrian doctrine, or is it a twentieth-century customary accretion consolidated by panchayat resolution and colonial-era codification?',
    'Textual-historical analysis separating Avestan and Vendidad provisions and classical practice from twentieth-century punchayet resolutions and the drafting history of the Parsi Marriage and Divorce Act, 1936.',
    'If customary accretion, the asymmetry is enforcement without doctrinal warrant and the extraction component sharpens toward the snare end for that seat; if doctrinal, it belongs to the coordination cost the reading itself prices in.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gender_asymmetry_doctrinal_basis, empirical, 'Doctrinal versus constructed origin of the gender-asymmetric descent bar.').

omega_variable(
    endogamy_survival_efficacy,
    'Does endogamy actually serve the founding problem — community survival — or does it accelerate decline by shrinking the marriage pool below replacement?',
    'Demographic cohort modeling comparing orthodox-center trajectories against liberalized diaspora trajectories across two generations, controlling for emigration and fertility.',
    'If endogamy accelerates decline, the constraint fails its own coordination test and the mandate-outlived-function condition ripens (piton-ward drift, capture/zombie flag risk); if it preserves, the measured extraction is the price of the coordination itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(endogamy_survival_efficacy, empirical, 'Whether the constraint''s coordination claim survives its own demographic record.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the measured suppression structural (withdrawal of ritual access, trust eligibility, family standing) or internalized (members experience out-marriage as betrayal of ancestors and the faith)?',
    'Post-exit suppression trajectory: compare members beyond the sanction''s reach (settled diaspora, lapsed families) — if obligation and guilt persist where material sanctions have lapsed, part of the suppression travels with the agent.',
    'If substantially internalized, effective suppression exceeds the structural measure and exit is costlier than the sanction stack alone implies; classification of the payer seats shifts accordingly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Structural versus internalized suppression mechanism in a tight-knit identity community.').

omega_variable(
    cs_kernel_framing_underdetermination,
    'Is the kernel of this reading the revealed text (Vendidad and Avesta, as codified in the 1936 Act) or the community''s custodial practice itself — and does the choice change the commitment-system classification?',
    'Test the alternative framing: under a practice-grounded framing, authority_grounding moves from lineage to practice, the interpretive layer dissolves into the practitioner body, and the drift vector reads as ordinary custom evolution rather than departure from a fixed reference.',
    'The declared framing (fixed_text/lineage) makes rising intermarriage read as drift from revelation; the practice framing makes the same facts read as the tradition legitimately evolving — the drift classification, though not the extraction measurement, depends on the choice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cs_kernel_framing_underdetermination, conceptual, 'Two coherent framings of the kernel produce different commitment-system outputs.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(family_law_authority__parsi_zoroastrian_reading, 0, 90).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fami_tr_t0, family_law_authority__parsi_zoroastrian_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(fami_tr_t0, observed).
narrative_ontology:measurement(fami_tr_t15, family_law_authority__parsi_zoroastrian_reading, theater_ratio, 15, 0.14).
narrative_ontology:measurement_basis(fami_tr_t15, observed).
narrative_ontology:measurement(fami_tr_t30, family_law_authority__parsi_zoroastrian_reading, theater_ratio, 30, 0.16).
narrative_ontology:measurement_basis(fami_tr_t30, observed).
narrative_ontology:measurement(fami_tr_t45, family_law_authority__parsi_zoroastrian_reading, theater_ratio, 45, 0.19).
narrative_ontology:measurement_basis(fami_tr_t45, observed).
narrative_ontology:measurement(fami_tr_t60, family_law_authority__parsi_zoroastrian_reading, theater_ratio, 60, 0.22).
narrative_ontology:measurement_basis(fami_tr_t60, observed).
narrative_ontology:measurement(fami_tr_t75, family_law_authority__parsi_zoroastrian_reading, theater_ratio, 75, 0.25).
narrative_ontology:measurement_basis(fami_tr_t75, observed).
narrative_ontology:measurement(fami_tr_t90, family_law_authority__parsi_zoroastrian_reading, theater_ratio, 90, 0.28).
narrative_ontology:measurement_basis(fami_tr_t90, observed).

% Extraction over time
narrative_ontology:measurement(fami_be_t0, family_law_authority__parsi_zoroastrian_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement_basis(fami_be_t0, observed).
narrative_ontology:measurement(fami_be_t15, family_law_authority__parsi_zoroastrian_reading, base_extractiveness, 15, 0.42).
narrative_ontology:measurement_basis(fami_be_t15, observed).
narrative_ontology:measurement(fami_be_t30, family_law_authority__parsi_zoroastrian_reading, base_extractiveness, 30, 0.47).
narrative_ontology:measurement_basis(fami_be_t30, observed).
narrative_ontology:measurement(fami_be_t45, family_law_authority__parsi_zoroastrian_reading, base_extractiveness, 45, 0.52).
narrative_ontology:measurement_basis(fami_be_t45, observed).
narrative_ontology:measurement(fami_be_t60, family_law_authority__parsi_zoroastrian_reading, base_extractiveness, 60, 0.57).
narrative_ontology:measurement_basis(fami_be_t60, observed).
narrative_ontology:measurement(fami_be_t75, family_law_authority__parsi_zoroastrian_reading, base_extractiveness, 75, 0.63).
narrative_ontology:measurement_basis(fami_be_t75, observed).
narrative_ontology:measurement(fami_be_t90, family_law_authority__parsi_zoroastrian_reading, base_extractiveness, 90, 0.68).
narrative_ontology:measurement_basis(fami_be_t90, observed).

% Suppression requirement over time
narrative_ontology:measurement(fami_su_t0, family_law_authority__parsi_zoroastrian_reading, suppression_requirement, 0, 0.44).
narrative_ontology:measurement_basis(fami_su_t0, observed).
narrative_ontology:measurement(fami_su_t15, family_law_authority__parsi_zoroastrian_reading, suppression_requirement, 15, 0.47).
narrative_ontology:measurement_basis(fami_su_t15, observed).
narrative_ontology:measurement(fami_su_t30, family_law_authority__parsi_zoroastrian_reading, suppression_requirement, 30, 0.51).
narrative_ontology:measurement_basis(fami_su_t30, observed).
narrative_ontology:measurement(fami_su_t45, family_law_authority__parsi_zoroastrian_reading, suppression_requirement, 45, 0.54).
narrative_ontology:measurement_basis(fami_su_t45, observed).
narrative_ontology:measurement(fami_su_t60, family_law_authority__parsi_zoroastrian_reading, suppression_requirement, 60, 0.57).
narrative_ontology:measurement_basis(fami_su_t60, observed).
narrative_ontology:measurement(fami_su_t75, family_law_authority__parsi_zoroastrian_reading, suppression_requirement, 75, 0.61).
narrative_ontology:measurement_basis(fami_su_t75, observed).
narrative_ontology:measurement(fami_su_t90, family_law_authority__parsi_zoroastrian_reading, suppression_requirement, 90, 0.64).
narrative_ontology:measurement_basis(fami_su_t90, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(family_law_authority__parsi_zoroastrian_reading, identity_coordination).
narrative_ontology:affects_constraint(family_law_authority__parsi_zoroastrian_reading, hindu_dharmashastra_reading).
narrative_ontology:affects_constraint(family_law_authority__parsi_zoroastrian_reading, muslim_shariat_reading).
narrative_ontology:affects_constraint(family_law_authority__parsi_zoroastrian_reading, christian_canonical_reading).
narrative_ontology:affects_constraint(family_law_authority__parsi_zoroastrian_reading, secular_contractual_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'religious family law' decomposes, per the epsilon-invariance principle, into five structurally distinct constraints — one per reading of the family_law_authority kernel. Each reading authors its own epsilon over its own standing arrangement: this story authors the Parsi endogamy regime as the Zoroastrian reading assesses it (substantial extraction concentrated on out-married women and their children, embedded in a genuine micro-minority survival function); the secular_contractual_reading authors a near-zero-extraction consent regime over a different arrangement. The stories form a constraint family linked through affects_constraints: the scriptural readings lend one another mutual legitimation, while the secular reading exerts exit-pressure on all of them through the availability of civil marriage.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
