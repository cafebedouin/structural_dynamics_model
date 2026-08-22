% ============================================================================
% CONSTRAINT STORY: permissive_license_text__corporate_moat_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_permissive_license_text__corporate_moat_reading, []).

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
 *   constraint_id: permissive_license_text__corporate_moat_reading
 *   human_readable: Permissive License Text as Corporate Moat — Uncompensated Proprietary Derivation
 *   domain: economic/technological/legal
 *
 * SUMMARY:
 *   A large share of the software commons is governed by permissive license
 *   texts — the MIT/BSD/Apache lineage — under which any party may use,
 *   modify, embed, and close derivative works, with attribution as the only
 *   standing obligation. This file instantiates one reading of that
 *   arrangement (see kernel_context): the regime as an
 *   uncompensated-appropriation channel through which enterprise corporations
 *   convert commons labor into proprietary product value — managed cloud
 *   services, closed firmware, commercial platforms — while the volunteer
 *   producers of the underlying code bear maintenance, support, and security
 *   burdens without claim or compensation. The epsilon authored here is for
 *   the standing arrangement as this reading assesses it, never for any
 *   alternative the reading's holders might endorse. Constraint family note
 *   (epsilon-invariance decomposition): the colloquial label 'permissive
 *   licensing' covers three structurally distinct claims with different
 *   epsilon over the same referent — the commons-coordination claim and the
 *   copyleft-counterfactual claim are separate files linked through
 *   network.affects_constraints; this file carries only the corporate-moat
 *   claim. Claim and metrics are authored independently: the type is claimed
 *   from this reading's structural assessment; the metrics describe the
 *   regime's operation as this reading measures it.
 *
 * KEY AGENTS:
 *   - volunteer_maintainers: primary target (powerless/trapped) — bear the maintenance, support, and security burden of corporate-scale use of their projects, with no claim on the value taken
 *   - uncompensated_core_contributors: secondary target (powerless/constrained) — merged labor becomes irrevocably available to closed appropriation under the license text
 *   - enterprise_cloud_vendors: primary beneficiary (institutional/arbitrage) — monetize commons code in proprietary services at scale; also defend the arrangement against relicensing (secondary agenda-setting role)
 *   - device_and_embedded_firms: secondary beneficiary (powerful/mobile) — close permissive components into shipped products; obligations end at attribution
 *   - license_stewards_and_foundations: agenda setter (institutional/identity_locked) — administer the texts and the normative frame; lean beneficiary-ward through corporate funding dependence
 *   - copyleft_licensing_advocates: excluded seat (organized/constrained) — hold the reciprocity alternative but sit outside permissive-foundation governance
 *   - software_policy_researchers: analytical observer (analytical/analytical) — document contribution asymmetry without a governance seat
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(permissive_license_text__corporate_moat_reading, 0.65).
domain_priors:suppression_score(permissive_license_text__corporate_moat_reading, 0.6).
domain_priors:theater_ratio(permissive_license_text__corporate_moat_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(permissive_license_text__corporate_moat_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(permissive_license_text__corporate_moat_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(permissive_license_text__corporate_moat_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(permissive_license_text__corporate_moat_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(permissive_license_text__corporate_moat_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(permissive_license_text__corporate_moat_reading, snare).
narrative_ontology:human_readable(permissive_license_text__corporate_moat_reading, "Permissive License Text as Corporate Moat — Uncompensated Proprietary Derivation").
narrative_ontology:topic_domain(permissive_license_text__corporate_moat_reading, "economic/technological/legal").

domain_priors:requires_active_enforcement(permissive_license_text__corporate_moat_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(permissive_license_text__corporate_moat_reading, '152815d3-9e19-4ef7-9631-d89bc353348a').
narrative_ontology:cs_kernel_codification('152815d3-9e19-4ef7-9631-d89bc353348a', fixed_text).
narrative_ontology:cs_authority_grounding('152815d3-9e19-4ef7-9631-d89bc353348a', practice).
narrative_ontology:cs_interpretation_layer_present('152815d3-9e19-4ef7-9631-d89bc353348a').
narrative_ontology:cs_reading_relation('152815d3-9e19-4ef7-9631-d89bc353348a', permissive_license_text__commons_coordination_reading, coexists_with).
narrative_ontology:cs_reading_relation('152815d3-9e19-4ef7-9631-d89bc353348a', permissive_license_text__copyleft_counterfactual_reading, influences).
narrative_ontology:cs_axiom('152815d3-9e19-4ef7-9631-d89bc353348a', foundational, uncompensated_proprietary_derivation_is_extraction).
narrative_ontology:cs_axiom_status(uncompensated_proprietary_derivation_is_extraction, holdable).
narrative_ontology:cs_axiom_grounding('152815d3-9e19-4ef7-9631-d89bc353348a', uncompensated_proprietary_derivation_is_extraction, deontological).
narrative_ontology:cs_axiom('152815d3-9e19-4ef7-9631-d89bc353348a', secondary, license_freedom_does_not_extinguish_producer_claims).
narrative_ontology:cs_axiom_status(license_freedom_does_not_extinguish_producer_claims, holdable).
narrative_ontology:cs_axiom_grounding('152815d3-9e19-4ef7-9631-d89bc353348a', license_freedom_does_not_extinguish_producer_claims, instrumental).
narrative_ontology:cs_reference_frame('152815d3-9e19-4ef7-9631-d89bc353348a', unconditional_commons_grant).
narrative_ontology:cs_drift_state('152815d3-9e19-4ef7-9631-d89bc353348a', cloud_commodity_extraction_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('152815d3-9e19-4ef7-9631-d89bc353348a', '').
narrative_ontology:cs_kernel_id(permissive_license_text__corporate_moat_reading, permissive_license_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(permissive_license_text__corporate_moat_reading, enterprise_cloud_vendors).
narrative_ontology:constraint_beneficiary(permissive_license_text__corporate_moat_reading, device_and_embedded_firms).
narrative_ontology:constraint_victim(permissive_license_text__corporate_moat_reading, volunteer_maintainers).
narrative_ontology:constraint_victim(permissive_license_text__corporate_moat_reading, uncompensated_core_contributors).
narrative_ontology:constraint_vindicates(permissive_license_text__corporate_moat_reading, license_text_exhaustiveness_doctrine).
narrative_ontology:constraint_vindicates(permissive_license_text__corporate_moat_reading, permissive_licensing_superiority_norm).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate managed services and proprietary platforms built on permissively licensed commons code. They take the code under the license's plain terms, close their derivative products, and monetize them at scale; contributions back are selective and aligned with product roadmaps. When a project attempts to change its license to require reciprocity, they fund or build forks under the old terms and use customer, funding, and governance leverage to keep access open. Leaving any single project is trivial for them: capital and engineering reach let them shift across codebases, vendors, and license regimes at will.
narrative_ontology:constraint_stakeholder(permissive_license_text__corporate_moat_reading, enterprise_cloud_vendors, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(permissive_license_text__corporate_moat_reading, enterprise_cloud_vendors, agenda_setter).

% Ship routers, consumer electronics, and industrial equipment embedding permissively licensed components in closed firmware. Their obligation under the license text ends at attribution notices. Compensation or code return is rare, and product margins assume free use of the commons layer. They can switch codebases or suppliers with moderate effort and face little reputational exposure for taking without giving back.
narrative_ontology:constraint_stakeholder(permissive_license_text__corporate_moat_reading, device_and_embedded_firms, beneficiary,
    powerful, biographical, mobile, global).

% Maintain widely deployed infrastructure projects as unpaid or underpaid labor. Corporate-scale use multiplies their support burden, security exposure, and burnout risk while sponsorship income covers a small fraction of the work for most. The license text gives them no claim on the value others take. Relicensing requires owning or reclaiming copyright across thousands of scattered contributions, and attempts split communities and trigger corporate forks that continue under the old terms. Walking away means abandoning work they are personally identified with.
narrative_ontology:constraint_stakeholder(permissive_license_text__corporate_moat_reading, volunteer_maintainers, payer,
    powerless, biographical, trapped, global).

% Contribute significant engineering labor to permissively licensed projects outside employment that pays for it. The value of merged contributions is immediately and irrevocably available to closed appropriation under the license text; they cannot retroactively condition it. Stopping future contribution is possible but costs them standing, skill-building, and community membership, and recovers nothing already given.
narrative_ontology:constraint_stakeholder(permissive_license_text__corporate_moat_reading, uncompensated_core_contributors, payer,
    powerless, biographical, constrained, global).

% Administer the license texts, foundation governance, and the definitional authority over what counts as open source. They write interpretive guidance, host critical projects, and set the normative frame in which reciprocity demands are read as community-hostile. Their funding increasingly flows from the same corporations that take without giving back, and their institutional identity is constituted by stewardship of the permissive commons; abandoning that frame would dissolve the organization's reason for being as they understand it.
narrative_ontology:constraint_stakeholder(permissive_license_text__corporate_moat_reading, license_stewards_and_foundations, agenda_setter,
    institutional, generational, identity_locked, global).

% Hold a fully formed alternative built on reciprocity-required licensing and would contest the permissive norm at every governance venue. Permissive foundations' governance structures and corporate funding keep reciprocity proposals off the agenda; their objections live outside the room, in separate license families and projects they run themselves.
narrative_ontology:constraint_stakeholder(permissive_license_text__corporate_moat_reading, copyleft_licensing_advocates, excluded,
    organized, generational, constrained, global).

% Study contribution asymmetry, maintainer sustainability, and the economics of corporate open-source consumption. They publish the accounting the governance conversation avoids, hold no seat in license governance, and have no enforcement role.
narrative_ontology:constraint_stakeholder(permissive_license_text__corporate_moat_reading, software_policy_researchers, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(permissive_license_text__corporate_moat_reading, enterprise_cloud_vendors).
narrative_ontology:fixing_cost_class(permissive_license_text__corporate_moat_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Minimizes legal friction for software reuse: a fixed, text-level grant lets any party, commercial or not, use, modify, embed, and redistribute commons code without negotiation, enabling rapid diffusion, interoperability, and the bundling of commons components into products at scale.
% TRANSFER_FUNCTION: Moves engineering and maintenance value from volunteer maintainers and unpaid contributors to enterprise corporations that embed the code in proprietary products and monetized services; a much smaller counter-flow of sponsorship, selective contributions, and reputational credit moves back toward the commons.
% ABSENT_VOICES: Copyleft reciprocity advocates would contest the permissive norm at every governance venue but are structurally outside permissive-foundation decision-making; the future maintainer cohorts whose labor the arrangement presumes are absent from every sustainability conversation; downstream organizations that depend on maintained infrastructure have no seat in license governance at all.
% DISAPPEARANCE_RATIONALE: If the permissive regime vanished overnight and every grant became reciprocity-bound or proprietary, corporate products and cloud services built on commons code would face immediate licensing crisis or forced reciprocity, device firmware supply chains would renegotiate, and the commons would reorganize around compensated or copyleft models. The beneficiaries are organized around the arrangement's continuation, which is precisely what makes its persistence contested rather than natural.
% FOUNDING_PROBLEM: Software reuse from the 1980s through the 1990s was choked by proprietary licensing friction: every use, port, or redistribution required case-by-case negotiation, and universities and vendors hoarded code. The permissive texts were written to eliminate that friction — a text-level grant requiring nothing but attribution — so code could flow freely into any use.
% FOUNDING_PROBLEM_CORROBORATION: Outside the benefiting parties: the relicensing cohort itself (MongoDB, Elastic, HashiCorp, Redis, Sentry) attests the friction problem is solved and the transfer is the live fact — their license changes were justified by uncompensated cloud consumption, not reuse friction; academic and foundation-sponsored maintainer studies document contribution asymmetry and burnout rather than reuse barriers; package-registry adoption data corroborates that frictionless reuse is fully achieved. Enterprise vendors and permissive foundations attest the founding problem is live, but they are the arrangement's beneficiaries and their attestation is not corroboration.
narrative_ontology:disappearance_verdict(permissive_license_text__corporate_moat_reading, world_rearranges).
narrative_ontology:founding_problem_status(permissive_license_text__corporate_moat_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(permissive_license_text__corporate_moat_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(permissive_license_text__corporate_moat_reading, 'none', 1).
narrative_ontology:epsilon_provenance(permissive_license_text__corporate_moat_reading, 0.65, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(permissive_license_text__corporate_moat_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(permissive_license_text__corporate_moat_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(permissive_license_text__corporate_moat_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.65: substantial uncompensated value flows from commons producers to proprietary appropriators, but the arrangement also delivers a real coordination good (frictionless reuse), which is why epsilon sits at moderate-high rather than extreme. Suppression (0.60) is structural-plus-normative rather than enforcement-machinery coercion: the license text legally forecloses compensation claims; community norms read reciprocity demands as community-hostile; and corporate counter-mobilization (forks under old terms, governance leverage, customer-lock messaging) actively punishes exit attempts. The arrangement needs no machinery to permit taking, but it needs active defense against leaving — which is why requires_active_enforcement is authored true and what the suppression series tracks. Suppression is authored as a raw structural property, unscaled by power or scope; only extractiveness is engine-scaled. Theater (0.42) captures a growing performative layer — sponsorship-as-legitimacy, open-source-program-office activity, token contributions — over a real but sharply asymmetric contribution core. Accessibility_collapse (0.48): alternatives exist and are exercised, but the characteristic exit — relicensing — frequently fails to escape, because corporate-sustained forks continue the codebase under the old permissive terms; the alternative reproduces the arrangement. Resistance (0.55) is the relicensing wave, the fair-source movement, and post-open discourse: real, episodic, and so far unable to move the regime's center. Measurements run on one shared grid (calendar years 2000-2024, all three metrics at every point). The dynamics are a ratcheting cycle rather than smooth drift: each relicensing episode (MongoDB 2018, Elastic 2021, HashiCorp 2023, Redis 2024) steps suppression up, resistance spikes, the episode subsides, and the suppression floor stays permanently higher — the scalar series smooths the episodes into the ratchet they leave behind. Coalition note: the victims are numerous but individually powerless; coalition power (collective relicensing vehicles, maintainer associations, regulatory pressure) is the possibility this structure is most vulnerable to, and it is blunted by copyright fragmentation across thousands of contributors and by collective-action costs.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from identical structural data. From the enterprise seat the license text is freedom infrastructure it funds, contributes to selectively, and defends; the transfer is invisible because the text names no obligation. From the maintainer seat the same text is a one-way valve on labor: value leaves at corporate scale, obligation returns at none. From the steward seat the regime is a gift economy under siege by ingratitude, and reciprocity demands read as attacks on the commons itself — an identity-locked position, since the foundations have become the permissive commons administratively and abandoning the frame would dissolve their institutional self-conception. The suppressive force is experienced as nothing at all by the beneficiary seat, as background norm by the steward seat, and as foreclosure by the payer seats. The engine computes these divergences from power and exit atoms; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary and victim declarations drive d. enterprise_cloud_vendors (declared beneficiary, arbitrage-grade exit) derive near the full-beneficiary end: the arrangement subsidizes them and they can leave any project at will. device_and_embedded_firms (beneficiary, mobile exit) derive similarly low. volunteer_maintainers and uncompensated_core_contributors (declared victims, trapped and constrained exit) derive near the full-target end: their sunk contributions are irrevocably under the text and relicensing exits usually reproduce the flow. license_stewards_and_foundations derive mid-range and lean beneficiary-ward: they administer rather than collect, but corporate funding dependence and identity fusion with the permissive frame sit them off-center. copyleft_licensing_advocates and software_policy_researchers are excluded and analytical seats respectively and feed no transfer arithmetic. No directionality_overrides are authored: the role-plus-exit derivation captures every seat without correction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — proprietary licensing friction blocking code flow — is solved: reuse happens at planetary scale without negotiation. What persists is the transfer structure, and the gains concentrate in a named seat (enterprise_cloud_vendors) rather than diffusing, which keeps this a capture condition rather than transient neglect. Fixing is prohibitive for whoever could fix it: copyright fragmentation across thousands of contributors, community-split risk, corporate fork threats, and foundation funding dependence raise the price of change above what any single fixer bears. The founding_problem_status=dead plus world_rearranges mismatch is the zombie/capture flag this reading exists to surface. The family decomposition prevents two misclassifications: folding the commons sibling's coordination function into this file would mask the transfer and compute a rope; folding this file's transfer into the copyleft sibling would erase the friction-minimization that still genuinely operates. Each reading keeps its own epsilon over the same standing arrangement, and this file's classification is a claim about this reading, not about the label.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This file instantiates the corporate_moat_reading of the permissive_license_text kernel; the same standing arrangement read by commons_coordination_reading or copyleft_counterfactual_reading yields different epsilon, beneficiary structures, and computed types. Which reading a seat adopts is the dominant determinant of classification here — where in the structure is the disagreement actually located?',
    'Generate the two sibling stories over identical stakeholder atoms and compare computed per-seat classifications: divergence in computed type at matched seats locates the disagreement empirically; agreement on facts with divergent types locates it normatively.',
    'If the disagreement is fact-indexed (value-flow magnitude), corpus accounting can shrink it; if normatively indexed (freedom versus producer desert), the three readings persist as coexisting constraints and no measurement resolves the contest.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Committer-structure omega: one-of-three-readings status of this constraint and the locus of the kernel contest.').

omega_variable(
    sibling_commons_reading_delta,
    'What changes structurally under the commons_coordination_reading sibling? There the same license texts are friction-minimizing freedom infrastructure: implementers everywhere, corporations included, are beneficiaries, uncompensated flow is the price of diffusion, victims largely vanish, and epsilon drops toward the coordination floor with a rope-type computed classification.',
    'Author the sibling story and run the engine on matched structural data; compare seat-level classifications and effective extraction at the enterprise and maintainer seats.',
    'Establishes whether the corporate-moat classification is reading-indexed over shared facts or depends on disputing the facts themselves; the pair brackets the rope-to-snare range for the same arrangement.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_commons_reading_delta, conceptual, 'Structural delta if the commons_coordination_reading were instantiated instead.').

omega_variable(
    sibling_copyleft_reading_delta,
    'What changes structurally under the copyleft_counterfactual_reading sibling? It holds the permissive regime as the counterfactual failure case that makes viral reciprocity necessary: victims match this reading''s, but the referent extends to foregone reciprocity and the endorsed alternative is GPL-style licensing, shifting the classification question from whether transfer occurs to whether reciprocity is the only sufficient remedy.',
    'Author the sibling story and compare its victim set, epsilon, and computed type against this file; the copyleft reading''s necessity claim is empirically contingent on whether non-viral remedies (compensation schemes, dual licensing, fair source) hold the transfer below its threshold.',
    'If non-viral remedies demonstrably suffice, the copyleft sibling''s foundational axiom weakens and this reading''s remedy space widens; if they fail, the sibling''s necessity claim strengthens and this reading collapses toward it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_copyleft_reading_delta, conceptual, 'Structural delta if the copyleft_counterfactual_reading were instantiated instead.').

omega_variable(
    net_transfer_magnitude,
    'How large is the net uncompensated transfer from commons producers to proprietary derivative producers, after netting corporate contributions, sponsorships, and infrastructure gifts against commercialization revenue attributable to permissive components?',
    'Longitudinal contribution-provenance accounting: commit authorship crossed with employer affiliation, against revenue attributable to permissively licensed components in closed products and managed services.',
    'A small net transfer would drop epsilon toward tangled_rope territory and weaken the snare claim; a large net transfer confirms the classification and gives compensation remedies their force.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(net_transfer_magnitude, empirical, 'Magnitude of the uncompensated value flow this reading asserts.').

omega_variable(
    relicensing_exit_effectiveness,
    'When projects exit by relicensing to reciprocity or source-available terms, does the exit hold, or do corporate-sustained forks under the old permissive terms reproduce the uncompensated flow?',
    'Track post-relicensing contribution and commercialization flows for the relicensing cohort (MongoDB, Elastic, HashiCorp, Redis, Sentry): fork adoption, contribution migration, customer retention.',
    'If forks reproduce the flow, accessibility_collapse is higher than authored, exit is weaker than the atoms suggest, and the snare hardens; if exits hold, alternatives are viable and the authored suppression is partly over-measured.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(relicensing_exit_effectiveness, empirical, 'Whether the arrangement''s exit route actually escapes the transfer.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the measured suppression primarily structural (the license text forecloses compensation claims; corporate counter-mobilization punishes relicensing) or internalized (maintainers'' open-source ethos makes them treat compensation demands as betrayals of the commons, self-censoring without external pressure)?',
    'Post-relicensing maintainer trajectories and survey data: where projects could relicense (clean copyright, contributor license agreements in place) but maintainers decline, the internalized component is substantial.',
    'If substantially internalized, effective suppression exceeds the structural measure, exit is more identity-locked than authored, and remedies must target norms and maintainer self-conception rather than license text alone.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Structural versus internalized composition of the arrangement''s suppressive force.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(permissive_license_text__corporate_moat_reading, 2000, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(plcmr_tr_t2000, permissive_license_text__corporate_moat_reading, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(plcmr_tr_t2004, permissive_license_text__corporate_moat_reading, theater_ratio, 2004, 0.14).
narrative_ontology:measurement(plcmr_tr_t2008, permissive_license_text__corporate_moat_reading, theater_ratio, 2008, 0.19).
narrative_ontology:measurement(plcmr_tr_t2011, permissive_license_text__corporate_moat_reading, theater_ratio, 2011, 0.23).
narrative_ontology:measurement(plcmr_tr_t2014, permissive_license_text__corporate_moat_reading, theater_ratio, 2014, 0.27).
narrative_ontology:measurement(plcmr_tr_t2016, permissive_license_text__corporate_moat_reading, theater_ratio, 2016, 0.31).
narrative_ontology:measurement(plcmr_tr_t2018, permissive_license_text__corporate_moat_reading, theater_ratio, 2018, 0.34).
narrative_ontology:measurement(plcmr_tr_t2020, permissive_license_text__corporate_moat_reading, theater_ratio, 2020, 0.37).
narrative_ontology:measurement(plcmr_tr_t2022, permissive_license_text__corporate_moat_reading, theater_ratio, 2022, 0.4).
narrative_ontology:measurement(plcmr_tr_t2024, permissive_license_text__corporate_moat_reading, theater_ratio, 2024, 0.42).

% Extraction over time
narrative_ontology:measurement(plcmr_be_t2000, permissive_license_text__corporate_moat_reading, base_extractiveness, 2000, 0.36).
narrative_ontology:measurement(plcmr_be_t2004, permissive_license_text__corporate_moat_reading, base_extractiveness, 2004, 0.42).
narrative_ontology:measurement(plcmr_be_t2008, permissive_license_text__corporate_moat_reading, base_extractiveness, 2008, 0.48).
narrative_ontology:measurement(plcmr_be_t2011, permissive_license_text__corporate_moat_reading, base_extractiveness, 2011, 0.53).
narrative_ontology:measurement(plcmr_be_t2014, permissive_license_text__corporate_moat_reading, base_extractiveness, 2014, 0.57).
narrative_ontology:measurement(plcmr_be_t2016, permissive_license_text__corporate_moat_reading, base_extractiveness, 2016, 0.6).
narrative_ontology:measurement(plcmr_be_t2018, permissive_license_text__corporate_moat_reading, base_extractiveness, 2018, 0.62).
narrative_ontology:measurement(plcmr_be_t2020, permissive_license_text__corporate_moat_reading, base_extractiveness, 2020, 0.63).
narrative_ontology:measurement(plcmr_be_t2022, permissive_license_text__corporate_moat_reading, base_extractiveness, 2022, 0.64).
narrative_ontology:measurement(plcmr_be_t2024, permissive_license_text__corporate_moat_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(plcmr_su_t2000, permissive_license_text__corporate_moat_reading, suppression_requirement, 2000, 0.2).
narrative_ontology:measurement(plcmr_su_t2004, permissive_license_text__corporate_moat_reading, suppression_requirement, 2004, 0.24).
narrative_ontology:measurement(plcmr_su_t2008, permissive_license_text__corporate_moat_reading, suppression_requirement, 2008, 0.3).
narrative_ontology:measurement(plcmr_su_t2011, permissive_license_text__corporate_moat_reading, suppression_requirement, 2011, 0.35).
narrative_ontology:measurement(plcmr_su_t2014, permissive_license_text__corporate_moat_reading, suppression_requirement, 2014, 0.4).
narrative_ontology:measurement(plcmr_su_t2016, permissive_license_text__corporate_moat_reading, suppression_requirement, 2016, 0.44).
narrative_ontology:measurement(plcmr_su_t2018, permissive_license_text__corporate_moat_reading, suppression_requirement, 2018, 0.5).
narrative_ontology:measurement(plcmr_su_t2020, permissive_license_text__corporate_moat_reading, suppression_requirement, 2020, 0.53).
narrative_ontology:measurement(plcmr_su_t2022, permissive_license_text__corporate_moat_reading, suppression_requirement, 2022, 0.57).
narrative_ontology:measurement(plcmr_su_t2024, permissive_license_text__corporate_moat_reading, suppression_requirement, 2024, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(permissive_license_text__corporate_moat_reading, resource_allocation).
narrative_ontology:affects_constraint(permissive_license_text__corporate_moat_reading, permissive_license_text__commons_coordination_reading).
narrative_ontology:affects_constraint(permissive_license_text__corporate_moat_reading, permissive_license_text__copyleft_counterfactual_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'permissive licensing' decomposes into three structurally distinct claims over one standing arrangement (epsilon-invariance decomposition): commons_coordination_reading (epsilon near the coordination floor, rope-type), this corporate_moat_reading (epsilon 0.65, snare-type), and copyleft_counterfactual_reading (reciprocity-necessity counterfactual). This file is downstream of the commons claim in the family's citation structure — the freedom-maximization premise is what the corporate-moat reading says enables the transfer — and upstream of the copyleft claim, whose necessity argument consumes this reading's extraction record as evidence. All three files link through affects_constraints; each carries its own epsilon, beneficiary structure, and classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
